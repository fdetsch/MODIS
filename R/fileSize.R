#' Get Size of File(s)
#' 
#' @description 
#' Function for getting size of any files.
#' 
#' @param file \code{character} vector of file(s) with path.
#' @param units \code{character}, defaults to "B". Currently available options 
#' are "B", "KB", "MB", "GB" or "TB" for bites, kilo-, mega-, giga- and terabytes.
#' 
#' @return 
#' \code{numeric} vector of the same length as \code{file} (in \code{units}). 
#' Note that directories are excluded.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' \dontrun{
#' fileSize(list.files("./"))
#' }
#' 
#' @export fileSize
#' @name fileSize
fileSize <- function(file,units = "B") {
  
  units <- toupper(units)
  unit <- c(1,1024,1048576,1073741824,1073741824*1024) 
  names(unit) <- c("B","KB", "MB", "GB","TB")
  
  if (!units %in% names(unit)) {
    stop('unit must be one of: "B", "KB", "MB", "GB" or "TB"')
  } 
  
  file <- file.info(file)
  file <- file[!file$isdir,"size"]
  
  res <- file/unit[toupper(units)]
  return(res)
}
