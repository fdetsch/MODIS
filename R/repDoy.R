# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2012
# Licence GPL v3

 
repDoy <- function(pixX, layerDate = NULL, bias = 0)
{	
  if (is.null(layerDate))
  {
    layerDate <- extractDate(colnames(pixX),asDate=TRUE)    
  }	
	if (layerDate$call$asDate)
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
  
  
  
  
  
