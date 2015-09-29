# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2012
# Licence GPL v3

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

