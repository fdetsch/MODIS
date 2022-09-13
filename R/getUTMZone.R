#' Get UTM Zone
#' 
#' @description 
#' Get the UTM zone for a geographic area. Zones are identified based on the 
#' centroid coordinate pair of the specified input.
#' 
#' @param x Extent information, see [getTile()] and Details therein.
#' 
#' @return 
#' A `c("sf", "data.frame")` in 
#' [EPSG:4326](http://spatialreference.org/ref/epsg/wgs-84/) with relevant 
#' output UTM zone information.
#' 
#' @author 
#' Florian Detsch
#' 
#' @references 
#' NGA Geomatics (2018) Coordinate Systems. Available online 
#' [here](http://earth-info.nga.mil/GandG/update/index.php?dir=coordsys&action=coordsys#tab_utm)
#' (2022-09-13).
#' 
#' @seealso 
#' [getTile()], [rgeos::gCentroid()].
#' 
#' @examples 
#' MODIS:::getUTMZone("tanzania")
#' 
#' data(meuse)
#' pts = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
#' MODIS:::getUTMZone(pts)
#' 
#' @export getUTMZone
#' @name getUTMZone
getUTMZone <- function(x = NULL) {
  
  ## if 'x' is missing, select UTM tile(s) interactively
  if (is.null(x)) {
    out = selectUTMZone()

  ## else identify zone from geographic input  
  } else {
    
    prj = sp::CRS("+init=epsg:4326")
    
    # 'character' (file) method: read 'Spatial' or 'Raster' file
    if (inherits(x, "character") & length(x) == 1) {
      if (raster::extension(x) == '.shp') {
        x <- raster::shapefile(x)
      } else {
        if (file.exists(x)) {
          x <- raster::raster(x)
        }
      }
    }  
    
    # 'character' (map) method: get 'Spatial' boundaries
    if (inherits(x, "character")) {
      try(testm <- maps::map("worldHires", x, plot = FALSE), silent = TRUE)
      if (!exists("testm")) {
        stop(paste0("Country name not valid. Check availability/spelling, i.e. try if it works with: map('worldHires,'",x, "'), or use '?search4map' function"))
      }
      x <- maps::map("worldHires", x, plot = FALSE, fill = TRUE)
    }
    
    # 'sf' method: convert to 'Spatial'
    if (inherits(x, "sf")) {
      x = methods::as(x, "Spatial")
    }
    
    # 'Raster,Spatial' method
    if (inherits(x, c("Raster", "Spatial"))) {
      
      # if required, spTransform() extent object
      if (!raster::compareCRS(x, prj) & !is.na(raster::projection(x))) {
        x <- if (inherits(x, "Raster")) {
          raster::projectExtent(x, prj)
        } else {
          sp::spTransform(x, prj)
        }
      }
      
      if (inherits(x, "Raster")) {
        x = ext2spy(raster::extent(x), as_sf = FALSE)
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
    
    # calculate center coordinate
    ctr = rgeos::gCentroid(x)
    
    grd ="inst/external/UTM_Zone_Boundaries.rds"
    out = sf::st_join(sf::st_as_sf(ctr), readRDS(grd))
  }
  
  return(out)
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


### select target UTM zone interactively ----

selectUTMZone = function() {
  
  grd <- readRDS("inst/extdata/UTM_Zone_Boundaries.rds")
  # grd <- readRDS(system.file("extdata", "UTM_Zone_Boundaries.rds", package = "MODIS"))
  sel <- mapedit::selectFeatures(grd)
  
  return(sel)
}
