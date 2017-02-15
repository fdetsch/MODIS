### input projection -----

InProj <- function(product) {
  
  if (product$SENSOR[1] == "MODIS") {
    if (product$TYPE[1] == "Tile") {
      paste0(' -s_srs ', shQuote("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
    } else {
      paste0(' -s_srs ', shQuote("+proj=longlat +ellps=clrk66 +no_defs"))
    }
  } else NULL
}


### output projection -----

OutProj <- function(product, extent, opts = NULL, ...) {
  
  if (is.null(opts))
    opts <- combineOptions(...)
  
  cat("########################\n")
  if(!is.null(extent$target$outProj)) {
    outProj <- checkOutProj(extent$target$outProj, tool = "GDAL")
    cat("outProj          = ", outProj, " (derived from Raster*/Spatial* object)\n")
    
  } else {
    outProj <- checkOutProj(opts$outProj, tool = "GDAL")
    cat("outProj          = ", outProj, "\n")
  }
  
  if (outProj == "asIn") {
    if (product$SENSOR[1] == "MODIS") {
      if (product$TYPE[1] == "Tile") {
        outProj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
      } else {
        outProj <- "+proj=longlat +ellps=clrk66 +no_defs" # CMG proj
      }
    }  
  }
  
  paste0(' -t_srs ', shQuote(outProj))
}


### output pixel size -----

PixelSize <- function(extent, opts = NULL, ...) {
  
  if (is.null(opts))
    opts <- combineOptions(...)
  
  if(!is.null(extent$target$pixelSize)) {
    pixelSize <- extent$target$pixelSize
    cat("pixelSize        = ", pixelSize, " (derived from Raster* object)\n")
    
  } else {
    pixelSize <- opts$pixelSize
    cat("pixelSize        = ", pixelSize, "\n")
  } 
  
  if (pixelSize[1] != "asIn") {
    if (length(pixelSize) == 1) {
      paste(" -tr", pixelSize, pixelSize)
    } else {
      paste(" -tr", paste0(pixelSize, collapse = " "))
    }
  } else NULL
}


### resampling type -----

ResamplingType <- function(opts = NULL, ...) {
  
  if (is.null(opts))
    opts <- combineOptions(...)
  
  opts$resamplingType <- checkResamplingType(opts$resamplingType, tool = "gdal")
  
  cat("resamplingType   = ", opts$resamplingType, "\n")
  paste(" -r", opts$resamplingType)
}
  

### target extent -----

TargetExtent <- function(extent, outProj) {
  
  te <- NULL # if extent comes from tileV/H
  
  if (!is.null(extent$target$extent)) { # all extents but not tileV/H
    if (is.null(extent$target$outProj)) { # map or list extents (always LatLon)
      rx <- raster(extent$target$extent, crs = "+init=epsg:4326") 
      rx <- projectExtent(rx, outProj)
      rx <- extent(rx) 
    } else {
      rx <- extent$target$extent
    }
  } 
  
  if (is.null(extent$target)) {
    if(!is.null(extent$extent)) {
      rx <- raster(extent$extent, crs = "+init=epsg:4326") 
      rx <- projectExtent(rx, outProj)
      rx <- extent(rx) 
    }
  }
  
  if (exists("rx")) te <- paste(" -te", rx@xmin, rx@ymin, rx@xmax, rx@ymax)  
  return(te)
}


### block size -----

BlockSize <- function(opts = NULL, ...) {
  
  if (is.null(opts))
    opts <- combineOptions(...)
  
  if (is.null(opts$blockSize)) {
    NULL
  } else {
    opts$blockSize <- as.integer(opts$blockSize)
    paste0(" -co BLOCKYSIZE=", opts$blockSize)
  }
}


### output compression -----

OutputCompression <- function(opts = NULL, ...) {

  if (is.null(opts))
    opts <- combineOptions(...)
  
  if (is.null(opts$compression)) {
    " -co compress=lzw -co predictor=2"
  } else if (isTRUE(opts$compression)) {
    " -co compress=lzw -co predictor=2"
  } else {
    NULL
  }
}


### quiet output -----

QuietOutput <- function(opts = NULL, ...) {

  if (is.null(opts))
    opts <- combineOptions(...)
  
  ## if 'quiet = FALSE' or not available, show full console output
  if ("quiet" %in% names(opts)) {
    if (opts$quiet) " -q" else NULL
  } else {
    NULL
  }
}