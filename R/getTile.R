#' Get MODIS Tile ID(s)
#' 
#' @description 
#' Get MODIS tile ID(s) for a specific geographic area.
#' 
#' @param x Extent information, see Details. Ignored if \code{tileH} and
#' \code{tileV} are specified.
#' @param tileH,tileV \code{numeric} or \code{character}. Horizontal and 
#' vertical tile number(s) of the 
#' \href{https://nsidc.org/data/docs/daac/mod10_modis_snow/landgrid.html}{MODIS Sinusoidal grid}
#' (e.g., \code{tileH = 1:5}). If specified, no cropping is performed and the 
#' full tile(s) (if more than one then also mosaicked) is (are) processed! 
#' @param mode Interactive selection mode as \code{character}. Available options 
#' are \code{"click"} (default) and \code{"draw"} that trigger interactive MODIS 
#' tile selection and free feature drawing, respectively. Ignored if any of 'x' 
#' or 'tileH,tileV' is NOT missing.
#' @param ... Additional arguments passed to \code{\link{MODISoptions}}. Here, 
#' only 'outProj' and 'pixelSize' are relevant, and this only if 'x' is an 
#' object of class \code{character}, \code{map}, \code{Extent} or \code{bbox}.
#' 
#' @return 
#' A \code{MODISextent} object.
#' 
#' @author 
#' Matteo Mattiuzzi, Florian Detsch
#' 
#' @seealso 
#' \code{\link{extent}}, \code{\link[sf]{st_bbox}}, \code{\link{map}}, 
#' \code{\link{search4map}}.
#' 
#' @note 
#' \strong{MODIS} does no longer support the tile identification and automated 
#' download of MERIS and SRTM data. At least as far as the latter is concerned, 
#' easy data access is granted through \code{\link{getData}}. 
#' 
#' @details 
#' If \code{x} is of class (see Examples for use cases)
#' \tabular{ll}{
#'   \code{missing}:\cr
#'   \tab If 'tileH,tileV' are specified, 'x' will be ignored. If no such tile 
#'   indices are provided and 'x' is missing, a viewer window pops up that 
#'   allows interactive tile selection from the global MODIS Sinusoidal grid or, 
#'   if \code{mode = "draw"}, free feature drawing.\cr
#'   \cr 
#'   \code{character}:\cr
#'   \tab Either the country name of a \code{map} object (see \code{\link{map}})
#'   or a valid file path of a raster image or ESRI shapefile (shp). The former 
#'   approach also supports pattern matching via regular expressions.\cr
#'   \cr
#'   \code{Raster*}:\cr
#'   \tab Spatial extent, resolution, and projection of the specified 
#'   \code{Raster*} are determined automatically. This information is used by 
#'   \code{\link{runGdal}} to create perfectly matching files. If the 
#'   \code{Raster*} comes with no valid CRS, 
#'   \href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326} is assumed.\cr
#'   \cr
#'   \code{Extent}, \code{bbox}:\cr
#'   \tab Boundary coordinates from \code{Extent} and \code{bbox} objects are 
#'   assumed to be in \href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326} 
#'   as such objects have no projection information attached.\cr
#'   \cr
#'   Other:\cr
#'   \tab \code{Spatial}, \code{sf}, or \code{map} object.
#' }
#' 
#' @examples 
#' \dontrun{
#' # ex 1 ############
#' # interactive tile selection
#' getTile()
#' }
#' 
#' # ex 2: Spatial (taken from ?rgdal::readOGR) ############
#' dsn <- system.file("vectors/Up.tab", package = "rgdal")[1]
#' Up <- rgdal::readOGR(dsn, "Up")
#' getTile(Up)
#' 
#' # ex 3: sf ############
#' ifl <- system.file("shape/nc.shp", package = "sf")
#' nc <- sf::st_read(ifl, quiet = TRUE)
#' getTile(nc)
#' 
#' # ex 4: tileH,tileV ############
#' getTile(tileH = 18:19, tileV = 4)
#' 
#' # ex 5: Raster* with valid CRS ############
#' rst1 <- raster(xmn = 9.2, xmx = 17.47, ymn = 46.12, ymx = 49.3)
#' getTile(rst1)
#' 
#' # this also works for projected data
#' rst3 <- projectExtent(rst1, crs = "+init=epsg:32633")
#' getTile(rst3)
#' 
#' # ex 6: Raster* without CRS or, alternatively, Extent or bbox --> treated as EPSG:4326 ############
#' mat2 <- matrix(seq(180 * 360), byrow = TRUE, ncol = 360)
#' rst2 <- raster(mat2, xmn = -180, xmx = 180, ymn = -90, ymx = 90)
#' getTile(rst2)
#' getTile(extent(rst1))
#' getTile(sf::st_bbox(nc))
#' 
#' # ex 7: map names as returned by search4map() ############
#' getTile("Austria")
#' getTile(c("Austria", "Germany"))
#' 
#' # or search for specific map name patterns (use with caution):
#' m1 <- search4map("Per")
#' getTile(m1)
#' 
#' # or use 'map' objects directly (remember to use map(..., fill = TRUE)): 
#' m2 <- map("state", region = "new jersey", fill = TRUE)
#' getTile(m2)
#' 
#' @export getTile
#' @name getTile
getTile <- function(x = NULL, tileH = NULL, tileV = NULL, mode = c("click", "draw"), ...) {
 
  opts = combineOptions(...)
  
  # if 'x' is a Raster*/Spatial* and has a different CRS, the information is added here
  target <- NULL  
  
  ## if inputs are missing, select tile(s) interactively
  if (is.null(x) && (is.null(tileH) | is.null(tileV))) {
    x = mapSelect(mode = mode[1])
    tileH = x$h; tileV = x$v
    fromMap = TRUE
  } else {
    fromMap = FALSE
  }
  
  # ## recycle tile lengths to enable creation of tile pairs
  # if ((len_h <- length(tileH)) != (len_v <- length(tileV))) {
  #   if (len_h < len_v) {
  #     if (len_v %% len_h > 0) {
  #       stop("'tileV' is not a multiple of 'tileH'.")
  #     }
  #     tileH = rep(tileH, len_v / len_h)
  #   } else {
  #     if (len_h %% len_v > 0) {
  #       stop("'tileH' is not a multiple of 'tileV'.")
  #     }
  #     tileV = rep(tileV, len_h / len_v)
  #   }
  # }
  
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
      
      # if coord. ref. is missing, set to EPSG:4326
      target <- list(outProj = raster::projection(x)
                     , extent = if (pts_1 <- (inherits(x, "SpatialPoints") & length(x) == 1L)) {
                       NULL
                     } else {
                       raster::extent(x)
                     }, pixelSize = NULL) 
      
      if (inherits(x, "Raster"))
        target$pixelSize <- raster::res(x)
      
      # if required, spTransform() extent object
      if (!sp::identicalCRS(x, sr) & !is.na(target$outProj)) {
        x <- if (inherits(x, "Raster")) {
          raster::projectExtent(x, prj)
        } else {
          sp::spTransform(x, prj)
        }
      } else if (is.na(target$outProj)) {
        target$outProj <- prj@projargs
      }
      
    # 'sf' method  
    } else if (inherits(x, "sf")) {
      
      pts_1 <- grepl("POINT", sf::st_geometry_type(x))[1] & nrow(x) == 1L
      
      if (!fromMap) {
        target <- list(outProj = sf::st_crs(x)$proj4string
                       , extent = if (pts_1) NULL else raster::extent(sf::st_bbox(x)[c(1, 3, 2, 4)])
                       , pixelSize = NULL)
        
        x =  methods::as(x, "Spatial")
        if (!sp::identicalCRS(x, sr)) {
          x <- sp::spTransform(x, prj)
        }
        
      } else {
        x <- methods::as(x, "Spatial")
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
      
      spy = sp::spTransform(spy, if (opts$outProj == "asIn") {
        oprj 
      } else {
        if (!is.na(suppressWarnings(as.integer(opts$outProj)))) {
          opts$outProj = paste0("+init=epsg:", as.integer(opts$outProj))
        }
        sp::CRS(opts$outProj)
      })
      
      target <- list(outProj = opts$outProj
                     , extent = raster::extent(spy)
                     , pixelSize = opts$pixelSize)
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
    
    if (inherits(x, "Spatial")) {
      selected = sr[x, ]
    } else {
      selected = raster::crop(sr, x)
      if (is.null(selected)) {
        stop("Please assign a valid CRS to 'x' as it doesn't seem to be in EPSG:4326.")
      }
    }
    
    tileH  <- unique(selected@data$h)
    tileV  <- unique(selected@data$v)
    
    tiles <- as.character(apply(selected@data,1,function(x) {paste("h",sprintf("%02d",x[2]),"v",sprintf("%02d",x[3]),sep="")}))
    
    if (exists("pts_1")) {
      if (pts_1) {
        tt <- tiletable[(tiletable$ih %in% tileH) &
                          (tiletable$iv %in% tileV) & (tiletable$lon_min > -999), ]
        
        x <- raster::extent(c(min(tt$lon_min), max(tt$lon_max)
                              , min(tt$lat_min), max(tt$lat_max)))
      }
    }
  }
  
  result = methods::new("MODISextent"
                        , tile = tiles
                        , tileH = as.integer(tileH)
                        , tileV = as.integer(tileV)
                        , extent = raster::extent(x)
                        , system = "MODIS"
                        , target = target)
  
  return(result)
}



mapSelect <- function(mode = c("click", "draw")) {
  
  grd <- sf::st_as_sf(sr, quiet = TRUE)
  
  sel = if (mode[1] == "click") {
    mapedit::selectFeatures(grd)
  } else {
    mapedit::drawFeatures(mode = mode[1])
  }

  return(sel)
}

