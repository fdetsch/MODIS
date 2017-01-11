#' Extract Dates from (MODIS) Files
#' 
#' @description 
#' This function helps to extract dates from a vector of files.
#' 
#' @param files \code{character} vector of filenames from which to extract 
#' dates.
#' @param pos1 \code{integer}, start of date string in \code{files}.
#' @param pos2 \code{integer}, end of date string. 
#' @param asDate \code{logical}. If \code{TRUE}, the result is converted to a 
#' \code{Date} object.
#' @param format \code{character}, date format. Used only if \code{asDate = TRUE}. 
#' Defaults to MODIS date style (i.e., \code{"\%Y\%j"} for year and julian day). 
#' See \code{\link{strptime}} for modifications.
#' 
#' @return 
#' If \code{asDate = FALSE}, a \code{character} vector; else a \code{Date} 
#' vector extracted from \code{files}.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' \dontrun{
#' # example on HDF files
#' files <- c("MOD13Q1.A2010209.h18v03.005.2010239071130.hdf",
#'            "MOD13Q1.A2010225.h18v03.005.2010254043849.hdf")
#' extractDate(files)
#' extractDate(files,asDate=TRUE)
#' 
#' # on any other file 
#' files <- c("Myfile_20010101.XXX","Myfile_20010115.XXX","Myfile_20010204.XXX")
#' extractDate(files,pos1=8,pos2=15)
#' extractDate(files,pos1=8,pos2=15,asDate=TRUE,format="\%Y\%m\%d")
#' }
#'  
#' @export extractDate
#' @name extractDate
extractDate <- function(files,pos1=10,pos2=16,asDate=FALSE,format="%Y%j")
{
  if(inherits(files,"Raster"))
  {
    files <- names(files)
  }
  files <- basename(files)
  date  <- sapply(files,function(x){substr(x,pos1,pos2)})
  if(asDate)
  {
    date <- as.Date(date, format=format)
    return(list(inputLayerDates = date, pos1=pos1, pos2=pos2, asDate = asDate, format=format))
  } else 
  {
    return(list(inputLayerDates = date, pos1 = pos1, pos2 = pos2, asDate = asDate))
  }
}

