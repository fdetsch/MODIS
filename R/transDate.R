#' MODIS Date Conversion and Testing
#'   
#' @description 
#' This function converts a sequence of input dates to 'YYYY-MM-DD' and 
#' 'YYYYDDD'.
#' 
#' @param begin \code{character}. Begin date(s) of time series, see 
#' \code{\link{transDate}}. If not provided, this defaults to 
#' \code{"1972-01-01"}.
#' @param end \code{character}. End date of MODIS time series. If not provided, 
#' this defaults to \code{Sys.Date()}.
#' 
#' @return 
#' A \code{list} of begin and end dates formatted according to 'YYYY-MM-DD' 
#' (first two slots; class \code{Date}) and 'YYYYDDD' (second two slots; class 
#' \code{character}).
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' transDate()
#' transDate(begin="2009.01.01")
#' transDate(end="2009.01.01")
#' transDate(begin="2009-01-01",end="2011.03.16")
#'                
#' @export transDate
#' @name transDate
transDate <- function(begin=NULL, end=NULL)
{
  if(is.null(begin)) 
  {
	  begin <- "1972.01.01" # Start with Landsat 1 
	}

  if (is.null(end)) 
  {
	  end <- strftime(Sys.Date(),"%Y.%m.%d") # actual date
  } 

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
  begin   <- as.Date(begin,format=paste("%Y",divisor,"%m",divisor,"%d",sep=""))

  if (is.na(begin)) 
  {
    stop("\n'begin=",begin,"' is eighter wrong format or a invalid date")
  }
	 divisor <- substr(end,5,5)
	 end     <- as.Date(end,format=paste("%Y",divisor,"%m",divisor,"%d",sep=""))

  if (is.na(end)) 
  {
    stop("\n'end=",end,"' is eighter wrong format or a invalid date")
  }

  if(end < begin)
  {
  	t     <- begin
	  begin <- end 
	  end   <- t	
	  rm(t)
  }

  beginDOY <- format(as.Date(begin,format="%Y.%m.%d"), "%Y%j")
  endDOY   <- format(as.Date(end,format="%Y.%m.%d"), "%Y%j")

  return(list(begin=begin,end=end,beginDOY=beginDOY,endDOY=endDOY))
}





