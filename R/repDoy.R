#' Repair MODIS "composite_day_of_the_year" SDS
#' 
#' @description 
#' Currently works only for MODIS 16 days composites! In MODIS composites, the 
#' Julian dates inside the 'composite_day_of_the_year' SDS are referring always 
#' to the year they are effectively in. The problem is that the layer/SDS name 
#' from the last files from Terra and Aqua within a year can include dates from 
#' the following year and so starting again with 1. The problem occurs if you 
#' want to sort values of a time series by date (e.g. for precise time series 
#' functions). This function generates a sequential vector beginning always 
#' with the earliest SDS/layer date and ending with the total sum of days of the
#' time series length.
#'  
#' @param pixX `matrix` of values, usually derived from [raster::as.matrix()].
#' @param layerDate If `NULL` (default), try to autodetect layer dates. If you 
#'   want to be sure, use the result from [extractDate()] or [orgTime()].
#' @param bias `integer`. Bias applied to all values in 'pixX'.
#'  
#' @return 
#' A `matrix` with sequential Julian dates.
#'  
#' @author 
#' Matteo Mattiuzzi
#'  
#' @examples 
#' \dontrun{
#' tfs <- runGdal(product="M.D13A2", begin="2010350", end="2011016"
#'                , extent="Luxembourg", job="deleteme", SDSstring="100000000010")
#' 
#' ndviFiles <- grep("NDVI.tif$", unlist(tfs, use.names = FALSE), value = TRUE)
#' ndviFiles <- preStack(files = ndviFiles, timeInfo = orgTime(ndviFiles))
#' ndvi <- stack(ndviFiles)
#' 
#' doyFiles <- grep("composite_day_of_the_year.tif$"
#'                  , unlist(tfs, use.names = FALSE), value = TRUE)
#' doyFiles <- preStack(files = doyFiles, timeInfo = orgTime(doyFiles))
#' doy <- stack(doyFiles)
#' 
#' layerDates <- extractDate(doyFiles)
#' 
#' pixX <- 169
#' 
#' y <- ndvi[pixX]
#' print(x1 <- doy[pixX])
#' print(x2 <- repDoy(x1,layerDates))
#' 
#' # the plotting example is not really good. 
#' # To create a figurative example it would be necessary to dolwnload to much data! 
#' plot("",xlim=c(1,max(x1,x2)),ylim=c(0,2000),xlab="time",ylab="NDVI*10000")
#' lines(y=y,x=x1,col="red",lwd=3)
#' lines(y=y,x=x2,col="green",lwd=2)
#' 
#' # repDoy function is thought to be embedded in something like that:
#' tr <- blockSize(ndvi)
#' 
#' doyOk <- brick(doy)
#' doyOk <- writeStart(doyOk, filename='test.tif',  overwrite=TRUE)
#' 
#' for (i in 1:tr$n)
#' {
#'   pixX  <- getValues(doy,tr$row[i],tr$nrows[i])
#'   ok    <- repDoy(pixX,layerDates)
#'   doyOk <- writeValues(x=doyOk,v=ok,start=tr$row[i])
#' }
#' doyOk <- writeStop(doyOk)
#' 
#' unlink(filename(doyOk))
#' }
#'  
#' @export repDoy
#' @name repDoy
repDoy <- function(pixX, layerDate = NULL, bias = 0)
{	
  if (is.null(layerDate))
  {
    layerDate <- extractDate(colnames(pixX),asDate=TRUE)    
  }	
	if (ifelse("call" %in% names(layerDate), layerDate$call$asDate, layerDate$asDate))
	{
	  layerDoy  <- format(layerDate$inputLayerDates,"%j")
    layerYear <- format(layerDate$inputLayerDates,"%Y")
  } else 
	{
    layerDoy  <- substr(layerDate$inputLayerDates,5,7)
    layerYear <- substr(layerDate$inputLayerDates,1,4)
	}

	if (is.matrix(pixX))
	{
   	pixX <- t(pixX)
	} else
	{
    pixX <- as.matrix(pixX) # if it is a vector
	}
	
  # de-bias 'end of year layers' that have 'start of year' dates 
  mask <- pixX - as.numeric(layerDoy)
  mask <- sign(mask)==-1
  mask[is.na(mask)] <- FALSE
  
  ndays      <- as.numeric(format(as.Date(paste0(layerYear,"-12-31")),"%j"))
  bias1      <- matrix(ndays, ncol = ncol(pixX), nrow = nrow(pixX), byrow=FALSE)
  pixX[mask] <- pixX[mask] + bias1[mask]  
	
  # sequentialize doys
  ndays   <- as.numeric(format(as.Date(paste0(unique(layerYear),"-12-31")),"%j"))
  bias1   <- cumsum(ndays) - ndays[1]
	counter <- as.numeric(table(layerYear)) # nlayers in Y
	
	biasN <- vector(mode="list",length=length(counter))
	for(i in seq_along(counter))
	{
	  biasN[[i]] <- rep(bias1[i],counter[i])
	}
	pixX <- pixX + unlist(biasN) + bias 
	return(t(pixX))
}
  
