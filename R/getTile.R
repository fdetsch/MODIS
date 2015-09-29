# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Licence GPL v3

getTile <- function(extent = NULL, tileH = NULL, tileV = NULL, buffer = NULL, system = "MODIS", zoom = TRUE)
{
  # debug:
  # extent = "austria"; tileH = NULL; tileV = NULL; buffer = NULL; system = "MODIS"; zoom=TRUE
  
  # if extent is a former result of getTile
  if (inherits(extent,"MODISextent"))
  {
    return(extent)
  }
  
  usePoly<- TRUE # "usePoly=F" works without suggested packages, "usePoly=T" only for MODIS system + having rgdal and rgeos installed
  target <- NULL  # if extent is a raster*/Spatial* and has a different proj it is changed
  system <- toupper(system)
  
  if (system == "MERIS")
  {
    # generate tiling structure of Culture-Meris data
    tiltab  <- genTile(tileSize = 5)
    usePoly <- FALSE
  } else if (system == "SRTM")
  {
    # generate tiling structure of SRTMv4 data
    tiltab  <- genTile(tileSize = 5, extent=list(xmin=-180, xmax=180, ymin=-60, ymax=60), StartNameFrom=c(1,1))
    usePoly <- FALSE
  } else 
  {
    if (! require(rgdal) )
    {
      cat("Using simple selection method,\n\tFor precise subsetting install the 'rgdal' package: install.packages('rgdal')\n")
      usePoly <- FALSE
      tiltab  <- tiletable
    }
    
    if (! require(rgeos) )
    {
      cat("Using simple selection method,\n\tFor precise subsetting install the 'rgeos' package: install.packages('rgeos')\n")
      usePoly <- FALSE
      tiltab  <- tiletable
    }
  }
  
  # supported extent: shp, list, raster-Extent/-Layer/-Brick/-Stack, map or blank
  # everything (except 'shp' and 'map') merges to an raster 'Extent' object
  # 'shp' and 'map' to a sp Polygon object (since the exact conture is used not the bounding box of the extent)
  # argument extent is prioritary to tileV/H.
  
  # if extent is a raster or a "path/name" to a rasterfile.
  if(inherits(extent,"character") & length(extent)==1)
  {
    if (file.exists(extent))
    {
      if (extension(extent)!= ".shp")
      {
        test <- try(raster(extent), silent=TRUE)
        if (!inherits(test,"try-error"))
        {
          extent  <- test
        }
      }
    }
  }
  
  # if extent is a shapefileNAME    
  test <- try(shapefile(extent), silent=TRUE)
  if (!inherits(test,"try-error"))
  {
    extent <- test
  }
    
  if (all(all(is.null(tileH) | is.null(tileV)) & is.null(extent))) # all NULL
  {
    extent <- mapSelect(zoom=zoom)
  }
  
  # if extent is expressed using tileV+H
  if(all(!is.null(tileH), !is.null(tileV), is.null(extent))) # use tileV/H
  {
    tileH <- as.numeric(tileH)
    tileV <- as.numeric(tileV)
    
    if (system == "MODIS")
    {
      tiltab <- tiletable
    }
    
    tt  <- tiltab[(tiltab$ih %in% tileH) & (tiltab$iv %in% tileV) & (tiltab$xmin>-999),]
    ext <- extent(c(min(tt$xmin),max(tt$xmax),min(tt$ymin),max(tt$ymax)))
    
    tt$iv <- sprintf("%02d", tt$iv)
    tt$ih <- sprintf("%02d", tt$ih)
    
    tileH <- sprintf("%02d",tileH)
    tileV <- sprintf("%02d",tileV)
    
    if (system == "SRTM")
    {
      tilesSUB <- as.character(apply(tt,1, function(x)
        {
          paste0("_", x[2], "_",x[1])}
        ))
      tiles <- as.character(sapply(tileH, function(x){paste0("_",  x, "_", tileV)}))
      
    } else if(system == "MODIS")
    {
      tilesSUB <- as.character(apply(tt,1, function(x)
        {
          paste0("h", x[2], "v",  x[1])
        }
        ))
      tiles <- as.character(sapply(tileH, function(x){paste0("h", x, "v", tileV)}))
    }
    
    if (!all(tiles %in% tilesSUB))  # all possible tiles vs all available
    {
      rem <- paste0(tiles[!tiles %in% tilesSUB],collapse=", ")
      cat(paste0("The following 'tiles' do not exist:\n",rem,"\n"))
      tiles <- tilesSUB
      tileH <- unique(tt$ih)
      tileV <- unique(tt$iv)
    }
    result <- list( tile = tiles, tileH = as.numeric(tileH), tileV = as.numeric(tileV), extent = extent, system = system, target=NULL)
    class(result) <- "MODISextent"
    return(result)
  }
  
  # if CHARACTER (country name of MAP)     
  if (inherits(extent, "character"))
  {
    if (!require(mapdata))
    {
      stop("For 'TILE' selection by country name (?map) you need to install the 'mapdata' package: install.packages('mapdata')")
    }
    
    try(testm <- map("worldHires", extent, plot = FALSE),silent = TRUE)
    if (!exists("testm"))
    {
      stop(paste0("Country name not valid. Check availability/spelling, i.e. try if it works with: map('worldHires,'",extent, "'), or use '?search4map' function"))
    }
    extent <- map("worldHires", extent, plot = FALSE, fill=TRUE)
  }
    
  # if MAP (from mapdata/maps)
  fromMap <- FALSE
  if (inherits(extent, "map"))
  {
    if (system == "MODIS")
    {
      extent  <- m2SP(extent, extent$names,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      fromMap <- TRUE
      usePoly <- TRUE            
    } else
    {
      extent <- extent(c(extent$range[1],extent$range[2],extent$range[3],extent$range[4]))
    }
  }

  # if raster* object or Spatial*
  if (length(grep(class(extent),pattern="Raster*"))==1 | length(grep(class(extent),pattern="Spatial*"))==1)
  {
    ext     <- extent
    outProj <- projection(ext)
    
    if (!isLonLat(ext)) 
    { 
      if (! require(rgdal) )
      {
        stop("Your 'extent' is not in LatLon coordinates, you need to install the 'rgdal' package: install.packages('rgdal')")
      }
      if(length(grep(class(extent),pattern="Raster*"))==1)
      {
        extent <- projectExtent(ext,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      } else
      {
        extent <- spTransform(ext,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      }
    }
    
    pixelSize <- NULL    
    if(length(grep(class(extent),pattern="Raster*"))==1)
    {
      pixelSize <- res(ext)
      extent    <- extent(extent)
    }
    if (fromMap) # this is required because if it exists it would overrule defaut 'outProj' in this case not desidered
    {
      outProj <- NULL
    }
    target <- list(outProj = outProj, extent = extent(ext), pixelSize = pixelSize) 
  }

  if (length(grep(class(extent), pattern="^SpatialPolygon*")==1) & !usePoly)
  {
    extent <- extent(extent)
  }
  # TODO; Quick and dirty
  if(length(grep(class(extent), pattern="^SpatialLine*"))==1 | length(grep(class(extent), pattern="^SpatialPoint*"))==1)
  {
    extent  <- extent(extent)
    usePoly <- FALSE
  }
  
  if (inherits(extent, "list")) 
  {
    if (length(extent$extent) == 4) 
    {
      extent <- extent$extent
    }
    extent <- extent(c(min(extent$xmin,extent$xmax), max(extent$xmin,extent$xmax), min(extent$ymin,extent$ymax), max(extent$ymin,extent$ymax)))
  }
  
  if (inherits(extent,"Extent"))
  {    
    # if min/max is inverted
    Txmax <- max(extent@xmin,extent@xmax)
    Txmin <- min(extent@xmin,extent@xmax)
    Tymax <- max(extent@ymin,extent@ymax)
    Tymin <- min(extent@ymin,extent@ymax)
    
    extent@ymin <- Tymin
    extent@ymax <- Tymax
    extent@xmin <- Txmin
    extent@xmax <- Txmax        
    
    if (!is.null(buffer)) 
    {
      if (length(buffer) == 1) 
      {
        buffer <- c(buffer, buffer)
      }
      extent@xmin <- extent@xmin - buffer[1]
      extent@xmax <- extent@xmax + buffer[1]
      extent@ymin <- extent@ymin - buffer[2]
      extent@ymax <- extent@ymax + buffer[2]
    }
  
  } else if (inherits(extent,"SpatialPolygonsDataFrame") & !is.null(buffer))
  {
    if (!require(rgeos))
    {
      stop("To use a 'buffer' in combination with an extent of class 'Spatial*' you need to install package 'rgeos': install.packages('rgeos')")
    }
    
    if (length(buffer)>1)
    {
      buffer <- buffer[1]
      warning(paste0("'buffer' on a vector object must have length==1. Using only the first element of 'buffer': ",buffer[1]))
    }
    
    # gBuffer doesn't allow buffer on LatLon, fake CRS bypass this. Found in: 
    # http://stackoverflow.com/questions/9735466/how-to-compute-a-line-buffer-with-spatiallinesdataframe
    win                 <- getOption("warn") 
    options(warn=-2)
    inproj              <- proj4string(extent)
    proj4string(extent) <- CRS("+init=epsg:3395")
    extent              <- gBuffer(extent,width=buffer)
    proj4string(extent) <- CRS(inproj)
    options(warn=win)
  }
  
  if(!usePoly)
  {
    if (system == "SRTM") 
    {
      if (extent@ymin >  60){stop("Latitudes are higer than SRTM coverage! Select an area inside Latitudes -60/+60\n")}
      if (extent@ymax < -60){stop("Latitudes are lower than SRTM coverage! Select an area inside Latitudes -60/+60\n")}
      if (extent@ymin < -60){extent@ymin <- -60; warning("Minimum Latitude is out of SRTM coverage, extent is trimmed to min LAT -60\n")}
      if (extent@ymax >  60){extent@ymax <-  60;  warning("Maximum Latitude is out of SRTM coverage, extent is trimmed to max LAT 60\n")}
    }
    
    minTile <- tiltab[((tiltab$xmin <= extent$xmin & tiltab$xmax >= extent$xmin) & (tiltab$ymin <= extent$ymin & tiltab$ymax >= extent$ymin)),c("iv","ih")]
    maxTile <- tiltab[((tiltab$xmin <= extent$xmax & tiltab$xmax >= extent$xmax) & (tiltab$ymin <= extent$ymax & tiltab$ymax >= extent$ymax)),c("iv","ih")]
    tiles   <- rbind(maxTile,minTile)
    
    tileV <- as.vector(min(tiles[1]):max(tiles[1]))
    tileH <- as.vector(min(tiles[2]):max(tiles[2]))
    
    vmax  <- max(tiltab$iv)
    hmax  <- max(tiltab$ih)
    
    if (min(tileH) < 0 || max(tileH) > hmax) 
    {
      stop(paste("'tileH' number(s) must be between 0 and",hmax, sep = ""))
    }
    if (min(tileV) < 0 || max(tileV) > vmax) 
    {
      stop(paste("'tileV' number(s) must be between 0 and",vmax, sep = ""))
    }
    
    vsize <- nchar(vmax)
    hsize <- nchar(hmax)
    tiles <- list()
    
    for (i in seq(along = tileH)) 
    {
      if (system == "SRTM")
      {
        tiles[[i]] <- paste("_", sprintf(paste("%0", hsize, "d", sep = ""), tileH[i]), "_", sprintf(paste("%0", hsize, "d", sep = ""), tileV), sep = "")
      } else 
      {
        tiles[[i]] <- paste("h", sprintf(paste("%0", hsize, "d", sep = ""), tileH[i]), "v", sprintf(paste("%0", hsize, "d", sep = ""), tileV), sep = "")                        
      }
    }
    result <- list(tile = unlist(tiles), tileH = tileH, tileV = tileV,extent = extent, system = system, target = target)
    class(result) <- "MODISextent"
    return(result)
    
  } else 
  {
    if(length(grep(class(extent),pattern="Spatial*"))==1) 
    {
      po <- vector(mode='list',length=length(extent))
      for(u in seq_along(extent))
      {
        po[[u]] <- Polygon(extent@polygons[[u]]@Polygons[[1]]@coords,hole=FALSE)
      }    
      extent <- extent(extent)
    } else
    {
      po <- list(Polygon(cbind(c(extent@xmin,extent@xmax,extent@xmax,extent@xmin,extent@xmin),c(extent@ymax,extent@ymax,extent@ymin,extent@ymin,extent@ymax)),hole=FALSE))
    }
    
    pos  <- Polygons(po,"selection")
    spos <- SpatialPolygons(list(pos))
    
    if (is.na(proj4string(spos)))
    {
      proj4string(spos) <- proj4string(sr) # sr
    }
     
    selected <- sr[spos,] # == rgeos:::over() # sr 
    
    tileH  <- unique(as.numeric(selected@data$h))
    tileV  <- unique(as.numeric(selected@data$v))
    result <- as.character(apply(selected@data,1,function(x) {paste("h",sprintf("%02d",x[2]),"v",sprintf("%02d",x[3]),sep="")}))
    result <- list(tile = result, tileH = tileH, tileV = tileV, extent = extent, system = system, target = target)
    class(result) <- "MODISextent"
    return(result)
  }
}


mapSelect <- function(zoom=TRUE)
{
  if (!require(mapdata)) 
  {
    stop("For interactive TILE selection you need to install the 'mapdata' package: install.packages('mapdata')")
  }
    
  dev.new(width=9,height=7)
  map("worldHires")
  map.axes() 
  grid(36,18,col="blue",lwd=0.5)
  abline(h=0,col="yellow",lwd=1)
  if(zoom) 
  {
    title("ZOOM-IN by selecting UL and LR points with the mouse!")            
    # code taken from function raster:::drawExtent
    loc1 <- locator(n = 1, type = "p", pch = "+", col = "red")
    loc2 <- locator(n = 1, type = "p", pch = "+", col = "red")
    loc  <- rbind(unlist(loc1), unlist(loc2))
    #
    p    <- rbind(c(min(loc[, "x"]), min(loc[,"y"])), c(min(loc[, "x"]), max(loc[, "y"])),
    c(max(loc[, "x"]), max(loc[, "y"])), c(max(loc[, "x"]), min(loc[,"y"])), c(min(loc[, "x"]),min(loc[,"y"])))
    lines(p, col = "red")            
    
    Sys.sleep(0.5)
    map("worldHires",xlim=c(min(loc[, "x"]),max(loc[, "x"])),ylim=c(min(loc[,"y"]),max(loc[, "y"])))
    map.axes() 
    grid(36,18,col="blue",lwd=0.5)            
  }
  title("Set UL and LR points with the mouse!")
  # code taken from function raster:::drawExtent
  loc1 <- locator(n = 1, type = "p", pch = "+", col = "red")
  loc2 <- locator(n = 1, type = "p", pch = "+", col = "red")
  loc  <- rbind(unlist(loc1), unlist(loc2))
  #
  p    <- rbind(c(min(loc[, "x"]), min(loc[,"y"])), c(min(loc[, "x"]), max(loc[, "y"])),
  c(max(loc[, "x"]), max(loc[, "y"])), c(max(loc[, "x"]), min(loc[,"y"])), c(min(loc[, "x"]),min(loc[,"y"])))
  lines(p, col = "red")
  
  extent <- extent(c(min(loc[, "x"]), max(loc[, "x"]), min(loc[, "y"]), max(loc[, "y"])))
  
  Sys.sleep(0.6)
  if(zoom)
  {
    text(x=min(loc[, "x"])+((max(loc[, "x"])-min(loc[, "x"]))/2),y=min(loc[,"y"])+((max(loc[, "y"])-min(loc[,"y"]))/2),"OK, extent is set!\nclosing window...",cex=3)    
  } else 
  {
    text(x=-4,y=21,"OK, extent is set!\nclosing window...",cex=5)
  }
  Sys.sleep(1.6)
  dev.off()
  return(extent)
}

# in order to avoid dependency the following functions are copied (nearly identical) from 'maptools' (v 0.8-26) package
# function 'map2SpatialPolygons' from package 'maptools' 
m2SP <- function (map, IDs, proj4string = CRS(as.character(NA))) 
{
  #require(maps)
  if (missing(IDs)) 
    stop("IDs required")
    xyList <- st1(cbind(map$x, map$y))
    if (length(xyList) != length(IDs)) 
        stop("map and IDs differ in length")
    tab <- table(factor(IDs))
    n <- length(tab)
    IDss <- names(tab)
    reg <- match(IDs, IDss)
    belongs <- lapply(1:n, function(x) which(x == reg))
    Srl <- vector(mode = "list", length = n)
    for (i in 1:n) {
        nParts <- length(belongs[[i]])
        srl <- vector(mode = "list", length = nParts)
        for (j in 1:nParts) {
            crds <- xyList[[belongs[[i]][j]]]
            if (nrow(crds) == 3) 
                crds <- rbind(crds, crds[1, ])
            srl[[j]] <- Polygon(coords = crds)
        }
        Srl[[i]] <- Polygons(srl, ID = IDss[i])
    }
    res <- as.SpatialPolygons.PolygonsList(Srl, proj4string = proj4string)
    res
}
# function 'map2SpatialLines' from package 'maptools' 
m2SL <- function (map, IDs = NULL, proj4string = CRS(as.character(NA))) 
{
#    require(maps)
    xyList <- st1(cbind(map$x, map$y))
    if (is.null(IDs)) 
        IDs <- as.character(1:length(xyList))
    if (length(xyList) != length(IDs)) 
        stop("map and IDs differ in length")
    tab <- table(factor(IDs))
    n <- length(tab)
    IDss <- names(tab)
    reg <- match(IDs, IDss)
    belongs <- lapply(1:n, function(x) which(x == reg))
    Srl <- vector(mode = "list", length = n)
    for (i in 1:n) {
        nParts <- length(belongs[[i]])
        srl <- vector(mode = "list", length = nParts)
        for (j in 1:nParts) {
            crds <- xyList[[belongs[[i]][j]]]
            if (nrow(crds) > 1) 
                srl[[j]] <- Line(coords = crds)
            else srl[[j]] <- Line(coords = rbind(crds, crds))
        }
        Srl[[i]] <- Lines(srl, ID = IDss[i])
    }
    res <- SpatialLines(Srl, proj4string = proj4string)
    res
}


# function '.NAmat2xyList' from package 'maptools' 
st1 <- function (xy) 
{
    NAs <- unclass(attr(na.omit(xy), "na.action"))
    if ((length(NAs) == 1L) && (NAs == nrow(xy))) {
        xy <- xy[-nrow(xy)]
        NAs <- NULL
    }
    diffNAs <- diff(NAs)
    if (any(diffNAs == 1)) {
        xy <- xy[-(NAs[which(diffNAs == 1)] + 1), ]
        NAs <- unclass(attr(na.omit(xy), "na.action"))
    }
    nParts <- length(NAs) + 1L
    if (!is.null(NAs) && nrow(xy) == NAs[length(NAs)]) 
        nParts <- nParts - 1
    res <- vector(mode = "list", length = nParts)
    from <- integer(nParts)
    to <- integer(nParts)
    from[1] <- 1
    to[nParts] <- nrow(xy)
    if (!is.null(NAs) && nrow(xy) == NAs[length(NAs)]) 
        to[nParts] <- to[nParts] - 1
    if (nParts > 1) {
        for (i in 2:nParts) {
            to[(i - 1)] <- NAs[(i - 1)] - 1
            from[i] <- NAs[(i - 1)] + 1
        }
    }
    for (i in 1:nParts) res[[i]] <- xy[from[i]:to[i], , drop = FALSE]
    res
}

