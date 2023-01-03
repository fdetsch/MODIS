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
#' @param x Extent information, see Details.
#' @param tileH,tileV `numeric` or `character`. Horizontal and vertical tile 
#' number(s) of the [MODIS Sinusoidal grid](https://modis-land.gsfc.nasa.gov/MODLAND_grid.html)
#' (e.g., `tileH = 1:5`). Cropping is disabled here and full tiles (if more than
#' one then also mosaicked) are processed instead. Ignored if 'x' is specified.
#' @param mode Interactive selection mode as `character`. Available options are 
#' `"click"` (default) and `"draw"` that trigger interactive MODIS tile 
#' selection and free feature drawing, respectively. Triggered only if 'x' and 
#' tile IDs are omitted.
#' @param ... Additional arguments passed to [MODISoptions()], see Details.
#' 
#' @return 
#' A `MODISextent` object.
#' 
#' @author 
#' Matteo Mattiuzzi, Florian Detsch
#' 
#' @seealso 
#' [raster::extent()], [sf::st_bbox()], [maps::map()], [search4map()].
#' 
#' @note 
#' **MODIS** does no longer support the tile identification and automated 
#' download of MERIS and SRTM data. At least as far as the latter is concerned, 
#' easy data access is granted through [raster::getData()]. 
#' 
#' @details 
#' Unless stated otherwise in the following, target 'outProj' and 'pixelSize' 
#' are carried over from [MODISoptions()].
#' 
#' If 'x' is of class (see Examples for use cases)
#' \tabular{ll}{
#'   `missing`:\cr
#'   \tab If tile IDs (see Arguments) are also missing, a viewer window 
#'   pops up that allows for interactive tile selection from the global MODIS 
#'   Sinusoidal grid or, if `mode = "draw"`, free feature drawing.\cr
#'   \cr 
#'   `character`:\cr
#'   \tab The country name of a `map` object (see [maps::map()]) with pattern 
#'   matching via regular expressions enabled. Alternatively, a valid file path 
#'   to a single ESRI shapefile (.shp) or an image readable by 
#'   [raster::raster()].\cr
#'   \cr
#'   `Raster*`:\cr
#'   \tab Spatial extent, resolution, and projection of the specified `Raster*` 
#'   are determined automatically. This information is used by [runGdal()] to 
#'   create perfectly matching files. If the `Raster*` comes with no valid CRS, 
#'   [EPSG:4326](https://spatialreference.org/ref/epsg/wgs-84/) is assumed.\cr
#'   \cr
#'   `Extent`, `bbox`:\cr
#'   \tab Boundary coordinates from `Extent` objects are generally assumed to be
#'   in [EPSG:4326](https://spatialreference.org/ref/epsg/wgs-84/) as such 
#'   objects have no projection information attached. The same applies for 
#'   `bbox` objects lacking CRS information.\cr
#'   \cr
#'   `sf`, `sfc`, `Spatial`:\cr
#'   \tab Except for resolution, same as for `Raster*`.\cr
#'   \cr
#'   Other:\cr
#'   \tab A `map` object.
#' }
#' 
#' @examples 
#' \dontrun{
#' # ex 1 ############
#' # interactive tile selection
#' getTile()
#' getTile(mode = "draw")
#' }
#' 
#' # ex 2: Spatial ############
#' dsn <- system.file("ex/lux.shp", package = "terra")
#' Up <- raster::shapefile(dsn, "Up")
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

### 0 INTERACTIVE ====

#' @aliases getTile,missing,missing,missing-method
#' @rdname getTile
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
    mode = match.arg(mode)
    x = mapSelect(mode = mode)
    # x = x[order(x$h, x$v), ]
    
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
    
    ftrs = mapedit::selectFeatures(
      sr[!sr_tls %in% tt_tls, ]
    )
    
    ## fail safe: no tile selected
    if (nrow(ftrs) == 0) {
      stop("No feature selected.")
    }
    
    return(ftrs)
    
    
    ### . draw mode ----
    
  } else {
    drawing = try(
      mapedit::drawFeatures()
      , silent = TRUE
    )
    
    ## fail safe: shape not closed
    if (inherits(drawing, "try-error")) {
      stop("Shape needs to be closed.")
    }
    
    ## disable use of s2 for spherical geometries
    use_s2 = modis_skip_s2()
    on.exit(
      modis_use_s2(
        use_s2
      )
    )
    
    suppressMessages(sr[drawing, ])
  }
}


### 1 TILES ==== 

setClassUnion("charORnum", c("character", "numeric"))

#' @aliases getTile,missing,charORnum,charORnum-method
#' @rdname getTile
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


### 2 CHARACTER ====

#' @aliases getTile,character-method
#' @rdname getTile
methods::setMethod(
  "getTile"
  , methods::signature(
    x = "character"
  )
  , function(
    x
    , ...
  ) {
    
    if (length(x) == 1 && file.exists(x)) { 
      if (raster::extension(x) == ".shp") {
        getTile(sf::st_read(x), ...)
      } else {
        getTile(raster::raster(x), ...)
      }
    } else {
      x.bu = x
      on.exit(rm(x.bu))
      
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
      }
      
      getTile(
        x
        , ...
      )
    }
  }
)


### 3 RASTER ====

#' @aliases getTile,Raster-method
#' @rdname getTile
methods::setMethod(
  "getTile"
  , methods::signature(
    x = "Raster"
  )
  , function(
    x
    , ...
  ) {
    
    # disable use of s2 for spherical geometries
    use_s2 = modis_skip_s2()
    on.exit(
      modis_use_s2(
        use_s2
      )
    )
    
    # if coord. ref. is missing, set to EPSG:4326
    if (is.na(raster::projection(x))) {
      raster::projection(x) = raster::projection(sr)
    }
    
    target = list(
      outProj = sf::st_crs(x)
      , extent = raster::extent(x)
      , pixelSize = raster::res(x)
    )
    
    # if required, project extent object
    if (sf::st_crs(x) != sf::st_crs(sr)) {
      x = raster::projectExtent(x, sr)
    }
    
    selected = suppressWarnings(
      suppressMessages(
        sf::st_crop(sr, x)
      )
    )
    
    if (nrow(selected) == 0) {
      stop("Please assign a valid CRS to 'x' as it doesn't seem to be in EPSG:4326.")
    }
    
    tiles = paste0(
      "h"
      , sprintf("%02d", selected$h)
      , "v"
      , sprintf("%02d", selected$v)
    )
    
    methods::new(
      "MODISextent"
      , tile = tiles
      , tileH = selected$h
      , tileV = selected$v
      , extent = raster::extent(x)
      , system = "MODIS"
      , target = target
    )
  }
)


### 4 MAP ====

methods::setOldClass("map")

#' @aliases getTile,map-method
#' @rdname getTile
methods::setMethod(
  "getTile"
  , methods::signature(
    x = "map"
  )
  , function(
    x
    , ...
  ) {
    
    opts = combineOptions(...)

    x = sf::st_transform(
      sf::st_as_sf(x)
      , if (opts$outProj == "asIn") {
        sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") 
      } else {
        if (!is.na(suppressWarnings(as.integer(opts$outProj)))) {
          opts$outProj = paste0("+init=epsg:", as.integer(opts$outProj))
        }
        sf::st_crs(opts$outProj)
      })
    
    getTile(
      sf::st_as_sf(x)
      , pixelSize = opts$pixelSize
    )
  }
)


### 4 BOUNDING BOX ====

### 4.0 Extent ----

#' @aliases getTile,Extent-method
#' @rdname getTile
methods::setMethod(
  "getTile"
  , methods::signature(
    x = "Extent"
  )
  , function(
    x
    , ...
  ) {
    
    getTile(
      sf::st_bbox(
        x
        , crs = sf::st_crs(sr)
      )
      , ...
    )
  }
)


### 4.1 bbox ----

methods::setOldClass("bbox")

#' @aliases getTile,bbox-method
#' @rdname getTile
methods::setMethod(
  "getTile"
  , methods::signature(
    x = "bbox"
  )
  , function(
    x
    , ...
  ) {
    
    opts = combineOptions(...)
    
    x = sf::st_as_sfc(x)
    
    # if coord. ref. is missing, set to EPSG:4326
    if (is.na(sf::st_crs(x))) {
      sf::st_crs(x) = sf::st_crs(sr)
    }
    
    x = sf::st_transform(
      x
      , if (opts$outProj == "asIn") {
        sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") 
      } else {
        if (!is.na(suppressWarnings(as.integer(opts$outProj)))) {
          opts$outProj = paste0("+init=epsg:", as.integer(opts$outProj))
        }
        sf::st_crs(opts$outProj)
      })
    
    getTile(
      sf::st_as_sf(x)
      , pixelSize = opts$pixelSize
    )
  }
)


### 5 SHAPEFILE ====

### 5.1 sp ----

#' @aliases getTile,Spatial-method
#' @rdname getTile
methods::setMethod(
  "getTile"
  , methods::signature(
    x = "Spatial"
  )
  , function(
    x
    , ...
  ) {
    
    getTile(sf::st_as_sf(x), ...)
  }
)


### 5.2 sf ----

#' @aliases getTile,sf-method
#' @rdname getTile
methods::setMethod(
  "getTile"
  , methods::signature(
    x = "sf"
  )
  , function(
    x
    , ...
  ) {
    
    opts = combineOptions(...)
    
    # disable use of s2 for spherical geometries
    use_s2 = modis_skip_s2()
    on.exit(
      modis_use_s2(
        use_s2
      )
    )
    
    # if coord. ref. is missing, set to EPSG:4326
    if (is.na(sf::st_crs(x))) {
      sf::st_crs(x) = sf::st_crs(sr)
    }
    
    # single-point feature -> take full tile extent
    pts_1 = grepl("POINT", sf::st_geometry_type(x))[1] && nrow(x) == 1L
    
    target = list(
      outProj = sf::st_crs(x)
      , extent = if (!pts_1) {
        raster::extent(x)
      }
      , pixelSize = opts$pixelSize
    )
    
    # if required, project extent object
    if (sf::st_crs(x) != sf::st_crs(sr)) {
      x = sf::st_transform(x, sf::st_crs(sr))
    }
    
    selected = suppressMessages(
      sf::st_filter(sr, x)
    )
    
    if (nrow(selected) == 0) {
      stop("Please assign a valid CRS to 'x' as it doesn't seem to be in EPSG:4326.")
    }
    
    tiles = paste0(
      "h"
      , sprintf("%02d", selected$h)
      , "v"
      , sprintf("%02d", selected$v)
    )
    
    methods::new(
      "MODISextent"
      , tile = tiles
      , tileH = selected$h
      , tileV = selected$v
      , extent = raster::extent(x)
      , system = "MODIS"
      , target = target
    )
  }
)


### 5.3 sfc ----

#' @aliases getTile,sfc-method
#' @rdname getTile
methods::setMethod(
  "getTile"
  , methods::signature(
    x = "sfc"
  )
  , function(
    x
    , ...
  ) {
    
    getTile(
      sf::st_as_sf(
        x
      )
      , ...
    )
  }
)