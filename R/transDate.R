#' MODIS Date Conversion and Testing
#'   
#' @description 
#' This function converts a sequence of input dates to 'YYYY-MM-DD' and 
#' 'YYYYDDD'.
#' 
#' @param begin,end \code{character} or \code{Date}. Begin (end) date of MODIS 
#' time series, see Note. If not provided, this defaults to \code{"1972-01-01"} 
#' (\code{Sys.Date()}).
#' 
#' @return 
#' A \code{list} of begin and end dates formatted according to 'YYYY-MM-DD' 
#' (first two slots; class \code{Date}) and 'YYYYDDD' (second two slots; class 
#' \code{character}).
#' 
#' @note 
#' If input dates are supplied as \code{character}, this function either expects 
#' 7-digit strings in the MODIS intrinsic form \code{'\%Y\%j'} or, alternatively, 
#' 10-digit strings in the form \code{'\%Y-\%m-\%d'} where the two field separators 
#' need to be uniform (see Examples).
#' 
#' @seealso \code{\link{strptime}}. 
#' 
#' @author 
#' Matteo Mattiuzzi, Florian Detsch
#' 
#' @examples 
#' transDate()
#' transDate(begin = "2009.01.01") # ends with current date
#' transDate(end = "2009.01.01") # starts with Landsat 1
#' transDate(begin = c("2009-01-01", "2010-01-01"), end = "2011.03.16")
#'                
#' @export transDate
#' @name transDate
transDate <- function(begin = NULL, end = NULL) {
  
  begin <- if (inherits(begin, "Date")) {
    format(begin, "%Y.%m.%d")
  } else if (is.null(begin)) {
    "1972.01.01" # start with Landsat 1 
  } else begin

  end <- if (inherits(end, "Date")) {
    format(end, "%Y.%m.%d")
  } else if (is.null(end)) {
	  format(Sys.Date(), "%Y.%m.%d") # current date
  } else end

  ## if 'begin' dates come in '%Y%j' format, reformat to '%Y.%m.%d'
  if (any(nchar(begin) == 7)) {
    
    # if all 'begin' dates have the same format, proceed
    if (all(nchar(begin) == 7)) {
      begin <- strftime(as.Date(begin, "%Y%j"), "%Y.%m.%d")
    
    # else throw error    
    } else {
      stop("Input dates are required to have the same format (e.g., '%Y%j').\n")
    }
  }
  
  ## same as above, but for 'end' dates
  if (any(nchar(end) == 7)) {
    
    if (all(nchar(end) == 7)) {
      end <- strftime(as.Date(end,"%Y%j"),"%Y.%m.%d")  
      
    } else {
      stop("Input dates are required to have the same format (e.g., '%Y%j').\n")
    }
  }

  divisor <- substr(begin,5,5)
  begin   <- as.Date(begin, format = paste0("%Y", divisor, "%m", divisor, "%d"))
  if (any(is.na(begin)))
    stop("'begin' date is either in a wrong format or an invalid date.\n")

  divisor <- substr(end,5,5)
  end     <- as.Date(end,format=paste("%Y",divisor,"%m",divisor,"%d",sep=""))
  if (any(is.na(end)))
    stop("'end' date is either in a wrong format or an invalid date.\n")

  if (any(end < begin)) {
    warning("'begin' and 'end' dates seem to be confused, reordering dates...\n")
    dts <- sort(c(begin, end))
    begin <- dts[1:(length(dts) - 1)]
    end <- dts[length(dts)]
  }

  beginDOY <- format(as.Date(begin,format="%Y.%m.%d"), "%Y%j")
  endDOY   <- format(as.Date(end,format="%Y.%m.%d"), "%Y%j")

  return(list(begin=begin,end=end,beginDOY=beginDOY,endDOY=endDOY))
}
