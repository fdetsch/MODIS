#' Extract Dates from (MODIS) Files
#' 
#' @description 
#' This function helps to extract dates from a vector of files.
#' 
#' @param files A `character` vector of filenames from which to extract dates. 
#'   Alternatively, a `Raster*` with date information in its [raster::names()].
#' @param pos1,pos2 Start and end of date string in 'files' as `integer`. If 
#'   missing, attempts to retrieve positions from a look-up table provided that 
#'   'files' comply with the MODIS standard naming convention.
#' @param asDate `logical`. If `TRUE`, the result is converted to a `Date` 
#'   object.
#' @param format `character`, date format. Used only if `asDate = TRUE`. 
#'   Defaults to MODIS date style (i.e., `"\%Y\%j"` for year and Julian day). 
#'   See [strptime()] for modifications.
#' 
#' @return 
#' A `list` with the following entries: 'inputLayerDates', 'pos1', 'pos2', 
#' 'asDate' and, optionally, 'format'. If `asDate = FALSE` (default), 
#' 'inputLayerDates' are represented as `character`, else as `Date`.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' # example on HDF files
#' files <- c("MOD13Q1.A2010209.h18v03.005.2010239071130.hdf",
#'            "MOD13Q1.A2010225.h18v03.005.2010254043849.hdf")
#' extractDate(files)
#' extractDate(files, asDate = TRUE)
#' 
#' # on any other file 
#' files <- c("Myfile_20010101.XXX", "Myfile_20010115.XXX", "Myfile_20010204.XXX")
#' extractDate(files, pos1 = 8, pos2 = 15)
#' extractDate(files, pos1 = 8, pos2 = 15, asDate = TRUE, format = "%Y%m%d")
#'  
#' @export extractDate
#' @name extractDate
extractDate <- function(files, pos1, pos2, asDate = FALSE, format = "%Y%j")
{
  if (inherits(files, "Raster")) {
    files <- names(files)
  }
  
  files <- basename(files)
  
  ## if any position indication is missing, try to retrieve it from look-up table
  if (any(missing(pos1), missing(pos2))) {
    ids = positionIndication(files)
    pos1 = ids[[1]]; pos2 = ids[[2]]
  }

  date  <- sapply(files,function(x){
    substr(x, pos1, pos2)
  })
  
  if(asDate)
  {
    date <- as.Date(date, format=format)
    return(list(inputLayerDates = date, pos1=pos1, pos2=pos2, asDate = asDate, format=format))
  } else 
  {
    return(list(inputLayerDates = date, pos1 = pos1, pos2 = pos2, asDate = asDate))
  }
}

