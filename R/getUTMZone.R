getUTMZone <- function(x = NULL, tileH = NULL, tileV = NULL) {
  
  opts = combineOptions(outProj = "UTM")
  
  ## if inputs are missing, select tile(s) interactively
  if (all(sapply(c(x, tileH, tileV), is.null))) {
    x = mapSelect()
    tileH = x$h; tileV = x$v
  }
  
  if (all(!is.null(tileH), !is.null(tileV))) {
    if (!is.numeric(tileH)) tileH <- as.numeric(tileH)
    if (!is.numeric(tileV)) tileV <- as.numeric(tileV)
    
    tt <- tiletable[(tiletable$ih %in% tileH) &
                      (tiletable$iv %in% tileV) & (tiletable$lon_min > -999), ]
    
    x <- raster::extent(c(min(tt$lon_min), max(tt$lon_max)
                          , min(tt$lat_min), max(tt$lat_max)))
    
    tt$iv <- sprintf("%02d", tt$iv)
    tt$ih <- sprintf("%02d", tt$ih)
    
    tileH <- sprintf("%02d",tileH)
    tileV <- sprintf("%02d",tileV)
    
    tilesSUB <- as.character(apply(tt,1, function(x) {
      paste0("h", x[2], "v",  x[1])
    }))
    tiles <- as.character(sapply(tileH, function(x){paste0("h", x, "v", tileV)}))
    
    if (!all(tiles %in% tilesSUB))  # all possible tiles vs all available
    {
      rem <- paste0(tiles[!tiles %in% tilesSUB],collapse=", ")
      cat(paste0("The following 'tiles' do not exist:\n",rem,"\n"))
      tiles <- tilesSUB
      tileH <- unique(tt$ih)
      tileV <- unique(tt$iv)
    }
  } else {
    
    fromMap <- FALSE
    prj <- sp::CRS(sp::proj4string(sr))
    oprj = sp::CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
    
    # filename string to Raster/vector conversion
    if(inherits(x,"character") & length(x)==1) # length>1 it should be only a mapname for maps::map
    {
      if (raster::extension(x)=='.shp')
      {
        x <- shapefile(x)
      } else 
      {
        if (file.exists(x))
        {
          x <- raster(x)
        }
      }
    }  
    
    # character (country name of MAP) to maps::map conversion     
    if (inherits(x, "character"))
    {
      try(testm <- maps::map("worldHires", x, plot = FALSE),silent = TRUE)
      if (!exists("testm"))
      {
        stop(paste0("Country name not valid. Check availability/spelling, i.e. try if it works with: map('worldHires,'",x, "'), or use '?search4map' function"))
      }
      x <- maps::map("worldHires", x, plot = FALSE, fill=TRUE)
    }
    
    # this needs to be done in order to use rgdal:::over to intersect geometies 
    if (inherits(x, c("Raster", "Spatial"))) {
      
      # if required, spTransform() extent object
      if (!raster::compareCRS(x, prj) & !is.na(raster::projection(x))) {
        x <- if (inherits(x, "Raster")) {
          raster::projectExtent(x, prj)
        } else {
          sp::spTransform(x, prj)
        }
      }
      
      # 'sf' method  
    } else if (inherits(x, "sf")) {
      
      if (!raster::compareCRS(sp::CRS(sf::st_crs(x)$proj4string), prj)) {
        x <- sf::st_transform(x, prj@projargs)
      }
      
      # 'map' | 'Extent' | 'bbox' method  
    } else if (inherits(x, c("map", "Extent", "bbox"))) {
      
      # convert to polygons
      if (inherits(x, "map")) {
        spy = x = maptools::map2SpatialPolygons(x, x$names, prj, checkHoles = TRUE)
      } else {
        if (inherits(x, "bbox")) {
          tmp = as.numeric(x)[c(1, 3, 2, 4)]
          x = raster::extent(tmp)
        }
        
        spy = as(x, "SpatialPolygons"); sp::proj4string(spy) = prj@projargs
      }
    }
    
    if (inherits(x, "sf"))
      x <- methods::as(x, "Spatial")
    
    ## capture errors related to orphaned holes and self-intersection
    if (inherits(x, 'Spatial')) {
      isValid = try(rgeos::gIsValid(x, reason = TRUE), silent = TRUE)
      if (inherits(isValid, "try-error")) {
        x = fixOrphanedHoles(x)
      } else if (inherits(isValid, "character")) {
        if (grepl("Self-intersection", isValid)) {
          x = suppressWarnings(
            rgeos::gBuffer(x, byid = TRUE, width = 0)
          )
        }
      }
    }
  }
  
  x = GEO2UTM(x)
  return(x)
}


### utm zone from geographic coordinates ----
### (adopted from https://www.wavemetrics.com/code-snippet/convert-yitudexgitude-utm)

GEO2UTM = function(x, y) {
  
  if (inherits(x, "Extent")) {
    x = as(x, "SpatialPolygons")
    sp::proj4string(x) = "+init=epsg:4326"
  }
  
  if (inherits(x, "Spatial")) {
    crd = sp::coordinates(x)
    x = crd[1]; y = crd[2]
  }
  
  # general zone
  zone = floor((x + 180)/6) + 1
  
  if (y >= 56.0 & y < 64.0 & x >= 3.0 & x < 12.0) {
    zone = 32
  }
  
  # special zones for Svalbard
  if (y >= 72.0 & y < 84.0) {
    zone = if (x >= 0.0 & x <  9.0) { 
      31
    } else if (x >= 9.0 & x < 21.0) {
      33
    } else if (x >= 21.0 & x < 33.0) {
      35
    } else if (x >= 33.0 & x < 42.0) {
      37
    }
  }
  
  return(zone)
}
