if (!isGeneric("getTile")) {
  setGeneric(
    "getTile"
    , function(
      x
      , tileH
      , tileV
      , ...
    ) {
      standardGeneric("getTile")
    }
  )
}

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
#' }
#' 
#' @export getTile
#' @name getTile

setClassUnion("charORnum", c("character", "numeric"))

methods::setMethod(
  "getTile"
  , methods::signature(
    x = "missing"
    , tileH = "charORnum"
    , tileV = "charORnum"
  )
  , function(
    tileH
    , tileV
    , ...
  ) {
    
    opts = combineOptions(
      ...
    )
    
    tileH = as.integer(tileH)
    tileV = as.integer(tileV)
    
    ih = iv = lon_min = lat_min = NULL
    tt = subset(
      tiletable
      , ih %in% tileH &
        iv %in% tileV & 
        lon_min > -999 & 
        lat_min > -99
    )
    
    ext = raster::extent(
      c(
        min(tt$lon_min)
        , max(tt$lon_max)
        , min(tt$lat_min)
        , max(tt$lat_max)
      )
    )
    
    tt$iv = sprintf("%02d", tt$iv)
    tt$ih = sprintf("%02d", tt$ih)
    
    tileH = sprintf("%02d", tileH)
    tileV = sprintf("%02d", tileV)
    
    tiles_sub = paste0("h", tt$ih, "v", tt$iv)
    
    hs = rep(tileH, each = length(tileV))
    vs = rep(tileV, times = length(tileH))
    tiles = paste0("h", hs, "v", vs)
    
    # all possible tiles vs. all available
    navl = !tiles %in% tiles_sub
    if (any(navl)) {
      warning(
        "The following tiles do not exist:\n"
        , paste(
          sort(
            tiles[navl]
          )
          , collapse = ", "
        )
        , "\n"
      )
      tiles = tiles_sub
      tileH = unique(tt$ih)
      tileV = unique(tt$iv)
    }
    
    methods::new(
      "MODISextent"
      , tile = tiles
      , tileH = as.integer(tileH)
      , tileV = as.integer(tileV)
      , extent = ext
      , system = "MODIS"
      , target = NULL
    )
  }
)


methods::setMethod(
  "getTile"
  , methods::signature(
    x = "missing"
    , tileH = "missing"
    , tileV = "missing"
  )
  , function(
    mode = c("click", "draw")
    , ...
  ) {
    x = mapSelect(mode = mode[1])
    x = x[order(x$h, x$v), ]
    
    tiles = paste0(
      "h", sprintf("%02d", x$h)
      , "v", sprintf("%02d", x$v)
    )
    
    methods::new(
      "MODISextent"
      , tile = tiles
      , tileH = x$h
      , tileV = x$v
      , extent = raster::extent(x)
      , system = "MODIS"
      , target = NULL
    )
  }
)

mapSelect = function(
  mode = c("click", "draw")
) {
  
  ### . click mode ----
  
  if (mode[1] == "click") {
    
    ## hide invalid tiles
    lon_min = lat_min = NULL
    tt_sbs = subset(
      tiletable
      , lon_min == -999 | 
        lat_min == -99
    )
    
    sr_tls = paste(sr$h, sr$v)
    tt_tls = paste(tt_sbs$ih, tt_sbs$iv)
    
    mapedit::selectFeatures(
      sr[!sr_tls %in% tt_tls, ]
    )
    
    ### . draw mode ----
    
  } else {
    mapedit::drawFeatures()
  }
}