# Author: Matteo Mattiuzzi, Anja Klisch, matteo.mattiuzzi@boku.ac.at
# Date : November 2011
# Licence GPL v3
  
transDate <- function(begin=NULL, end=NULL)
{
  if(is.null(begin)) 
  {
	  begin <- "1972.01.01" # Start with Landsat 1 
	}

  if (is.null(end)) 
  {
	  end <- format(Sys.time(),"%Y.%m.%d") # actual date
  } 

  if (nchar(begin)==7)
  {
	  begin <- format(as.Date(begin,"%Y%j"),"%Y.%m.%d")   
  }
  if (nchar(end)==7) 
  {
	  end <- format(as.Date(end,"%Y%j"),"%Y.%m.%d")  
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





