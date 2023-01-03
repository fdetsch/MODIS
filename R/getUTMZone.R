# Get UTM Zone
# 
# @description 
# Get the UTM zone for a geographic area. Zones are identified based on the 
# centroid coordinate pair of the specified input.
# 
# @param x Extent information, see [getTile()] and Details therein.
# 
# @return 
# A `c("sf", "data.frame")` in 
# [EPSG:4326](http://spatialreference.org/ref/epsg/wgs-84/) with relevant UTM 
# zone information.
# 
# @author 
# Florian Detsch
# 
# @seealso 
# [getTile()], [sf::st_centroid()].
# 
# @examples
# \dontrun{
# source("R/getUTMZone.R")
# 
# getUTMZone("tanzania")
# 
# data(meuse)
# pts = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
# getUTMZone(pts)
# }
# 
# @export
getUTMZone <- function(x = NULL) {
  
  ## if 'x' is missing, select UTM tile(s) interactively
  if (is.null(x)) {
    return(
      selectUTMZone()
    )
  }
  
  # `character` file method: read as `sf` or `Raster`
  if (inherits(x, "character") && file.exists(x[1])) {
    
    if (length(x) > 1L) {
      warning(
        sprintf(
          "Expected length of 'x' is [1L], got [%sL]. Dumping excess elements.."
          , length(x)
        )
        , call. = FALSE
      )
      
      x = x[1]
    }
    
    err = try(
      sf::st_read(x)
      , silent = TRUE
    )
    
    # early exit: input is neither {sf} nor {raster} compatible
    x = tryCatch(
      error = \(e) {
        stop(
          "'x' is compatible with neither {sf} nor {raster}"
          , call. = FALSE
        )
      }
      , raster::raster(x)
    )
  }
  
  # 'character' map method: get 'sf' boundaries
  if (inherits(x, "character")) {
    x = maps::map(
      "worldHires"
      , x
      , plot = FALSE
      , fill = TRUE
    )
  }
  
  # `Raster` method
  if (inherits(x, c("Raster", "Extent"))) {
    x = sf::st_as_sfc(
      sf::st_bbox(x)
    )
    
    if (is.na(sf::st_crs(x))) {
      warning(
        "Input coordinate reference system is unknown, assuming EPSG:4326.."
        , call. = FALSE
      )
      
      sf::st_crs(x) = sf::st_crs(4326L)
    }
  }
  
  # `Spatial,map,bbox` method
  if (inherits(x, c("Spatial", "map", "bbox"))) {
    x = sf::st_as_sf(
      x
    )
  }
  
  ## early exit: input geometry not valid
  stopifnot(
    "Input geometry is not valid, try to `sf::st_make_valid()` and try again." =
      sf::st_is_valid(x)
  )
  
  # determine centroid coordinates
  ctr = suppressWarnings(
    sf::st_centroid(x)
  )
  
  # if required, transform to epsg:4326
  grd = readRDS("inst/external/UTM_Zone_Boundaries.rds")
  
  if (isFALSE(sf::st_crs(grd) == sf::st_crs(ctr))) {
    ctr = sf::st_transform(
      ctr
      , sf::st_crs(grd)
    )
  }
  
  # return utm zone
  sf::st_join(
    ctr
    , grd
  )
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
