### input projection -----

InProj <- function(product) {
  if (product@TYPE[1] == "Tile") {
    "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
  } else {
    "+proj=longlat +ellps=clrk66 +no_defs"
  }
}


### output projection -----

OutProj <- function(product, extent, ...) {
  
  opts <- combineOptions(...)
  
  cat("########################\n")
  if(!is.null(extent@target$outProj)) {
    outProj <- checkOutProj(extent@target$outProj, tool = "GDAL")
    cat("outProj          = ", if (inherits(outProj, "crs")) outProj$proj4string else outProj, " (if applicable, derived from Raster*/Spatial*/sf* object)\n")
    
  } else {
    outProj <- checkOutProj(opts$outProj, tool = "GDAL")
    cat("outProj          = ", if (inherits(outProj, "crs")) outProj$proj4string else outProj, "\n")
  }
  
  if (outProj == "asIn") {
    if (product@TYPE[1] == "Tile") {
      outProj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
    } else {
      outProj <- "+proj=longlat +ellps=clrk66 +no_defs" # CMG proj
    }
  }
  
  if (inherits(outProj, "crs")) outProj$proj4string else outProj
}


### output pixel size -----

PixelSize <- function(extent, ...) {
  
  opts <- combineOptions(...)
  
  if(!is.null(extent@target$pixelSize)) {
    pixelSize <- extent@target$pixelSize
    cat("pixelSize        = ", pixelSize, " (if applicable, derived from Raster* object)\n")
    
  } else {
    pixelSize <- opts$pixelSize
    cat("pixelSize        = ", pixelSize, "\n")
  } 
  
  if (pixelSize[1] != "asIn") {
    if (length(pixelSize) == 1) {
      rep(pixelSize, 2)
    } else {
      pixelSize
    }
  }
}


### resampling type -----

ResamplingType <- function(...) {
  
  opts <- combineOptions(...)
  
  opts$resamplingType <- checkResamplingType(opts$resamplingType, tool = "gdal")
  
  cat("resamplingType   = ", opts$resamplingType, "\n")
  opts$resamplingType
}
  

### target extent -----

TargetExtent <- function(extent, outProj) {
  
  if (!is.null(extent@target$extent)) { # all extents but not tileV/H
    if (is.null(extent@target$outProj)) { # map or list extents (always LatLon)
      rx <- raster(extent@target$extent, crs = "+init=epsg:4326") 
      rx <- projectExtent(rx, outProj)
      rx <- extent(rx) 
    } else {
      rx <- extent@target$extent
    }
  } 
  
  if (is.null(extent@target)) {
    if(!is.null(extent@extent)) {
      rx <- raster(extent@extent, crs = "+init=epsg:4326")
      # suppress 'Discarded ... unknown in CRS definition' warning
      rx <- suppressWarnings(projectExtent(rx, outProj))
      rx <- extent(rx) 
    }
  }
  
  if (exists("rx")) {
    as.character(sf::st_bbox(rx))
  }
}


### block size -----

BlockSize <- function(...) {
  
  opts <- combineOptions(...)
  
  if (!is.null(opts$blockSize)) {
    opts$blockSize <- as.integer(opts$blockSize)
    paste0("BLOCKYSIZE=", opts$blockSize)
  }
}


### output compression -----

OutputCompression <- function(...) {

  opts <- combineOptions(...)
  
  if (is.null(opts$compression) || isTRUE(opts$compression)) {
    c("compress=lzw", "predictor=2")
  }
}


### quiet output -----

QuietOutput <- function(...) {

  opts <- combineOptions(...)
  
  ## if 'quiet = FALSE' or not available, show full console output
  if ("quiet" %in% names(opts)) {
    if (opts$quiet) "-q"
  }
}


### gdal drivers ----

getGdalDrivers = function() {
  sf::st_drivers(
    what = "raster"
  )
}

getGdalWriteDrivers = function() {
  subset(
    getGdalDrivers()
    , write
  )
}