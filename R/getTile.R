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
#'   \tab The country name of a \code{map} object (see \code{\link{map}}) with 
#'   pattern matching via regular expressions enabled. Alternatively, a valid 
#'   file path to an ESRI shapefile (.shp) or an image readable by 
#'   \code{\link[raster]{raster}}.\cr
#'   \cr
#'   \code{Raster*}:\cr
#'   \tab Spatial extent, resolution, and projection of the specified 
#'   \code{Raster*} are determined automatically. This information is used by 
#'   \code{\link{runGdal}} to create perfectly matching files. If the 
#'   \code{Raster*} comes with no valid CRS, 
#'   \href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326} is assumed.\cr
#'   \cr
#'   \code{Extent}, \code{bbox}:\cr
#'   \tab Boundary coordinates from \code{Extent} objects are generally assumed 
#'   to be in \href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326} as 
#'   such objects have no projection information attached. The same applies for 
#'   'bbox' objects lacking CRS information.\cr
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
    
    prj <- sf::st_crs(sr)
    oprj = sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

    # filename string to Raster/vector conversion
    if (inherits(x, "character") && length(x) == 1 && file.exists(x)) { # length>1 it should be only a mapname for maps::map
      x = if (raster::extension(x) == ".shp") {
        sf::st_read(x)
      } else {
        raster::raster(x)
      }
    }
    
    # character (country name of MAP) to maps::map conversion     
    if (inherits(x, "character")) {
      x.bu = x
      x = try(
        maps::map("worldHires", x, plot = FALSE, fill = TRUE)
        , silent = TRUE
      )
      if (inherits(x, "try-error")) {
        stop(
          "Country name not valid. Check availability with map('worldHires, '"
          , x.bu
          , "'), or use '?search4map' function"
        )
      } else {
        rm(x.bu)
      }
    }
    
    # this needs to be done in order to use rgdal:::over to intersect geometies 
    if (inherits(x, c("Spatial", "map"))) {
      x = sf::st_as_sf(x)
    }
    
    if (inherits(x, c("Extent", "bbox"))) {
      
      # convert to polygons
      if (inherits(x, "Extent")) {
        x = sf::st_bbox(
          x
          , crs = 4326
        )
      } else if (is.na(sf::st_crs(x))) {
        sf::st_crs(x) = 4326
      }
      
      x = sf::st_as_sfc(x)

      x = sf::st_transform(x, if (opts$outProj == "asIn") {
        oprj 
      } else {
        if (!is.na(suppressWarnings(as.integer(opts$outProj)))) {
          opts$outProj = paste0("+init=epsg:", as.integer(opts$outProj))
        }
        sf::st_crs(opts$outProj)
      })
    }
    
    is_sf = inherits(x, c("sf", "sfc"))
    
    pts_1 = FALSE
    if (is_sf) {
      pts_1 = grepl("POINT", sf::st_geometry_type(x))[1] && nrow(x) == 1L
    }
    
    # if coord. ref. is missing, set to EPSG:4326
    target = list(
      outProj = sf::st_crs(x)
      , extent = if (!pts_1) {
        raster::extent(sf::st_bbox(x)[c(1, 3, 2, 4)])
      }
      , pixelSize = if (!is_sf) {
        raster::res(x)
      } else {
        opts$pixelSize
      }
    )
    
    # if required, project extent object
    if (sf::st_crs(x) != sf::st_crs(sr) && !is.na(target$outProj)) {
      x = if (!is_sf) {
        raster::projectExtent(x, sr)
      } else {
        sf::st_transform(x, prj)
      }
    } else if (is.na(target$outProj)) {
      if (!is_sf) {
        raster::projection(x) = raster::projection(sr)
      } else {
        sf::st_crs(x) = prj
      }
      target$outProj = prj
    }
    
    # ## capture errors related to orphaned holes and self-intersection
    # if (inherits(x, 'Spatial')) {
    #   isValid = try(rgeos::gIsValid(x, reason = TRUE), silent = TRUE)
    #   if (inherits(isValid, "try-error")) {
    #     x = fixOrphanedHoles(x)
    #   } else if (inherits(isValid, "character")) {
    #     if (grepl("Self-intersection", isValid)) {
    #       x = suppressWarnings(
    #         rgeos::gBuffer(x, byid = TRUE, width = 0)
    #       )
    #     }
    #   }
    # }
    
    selected = suppressWarnings(
      suppressMessages(
        do.call(
          if (inherits(x, "Raster")) {
            sf::st_crop
          } else {
            sf::st_filter
          }
          , args = list(
            x = sr
            , y = x
          )
        )
      )
    )
    
    if (nrow(selected) == 0) {
      stop("Please assign a valid CRS to 'x' as it doesn't seem to be in EPSG:4326.")
    }
    
    tileH  <- unique(selected$h)
    tileV  <- unique(selected$v)
    
    tiles = paste0(
      "h"
      , sprintf("%02d", tileH)
      , "v"
      , sprintf("%02d", tileV)
    )
    
    if (pts_1) {
      tt <- tiletable[(tiletable$ih %in% tileH) &
                        (tiletable$iv %in% tileV) & (tiletable$lon_min > -999), ]
      
      x <- raster::extent(c(min(tt$lon_min), max(tt$lon_max)
                            , min(tt$lat_min), max(tt$lat_max)))
    }
  }

  result = methods::new("MODISextent"
                        , tile = tiles
                        , tileH = as.integer(tileH)
                        , tileV = as.integer(tileV)
                        , extent = raster::extent(sf::st_bbox(x)[c(1, 3, 2, 4)])
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

