# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2012
# Licence GPL v3

orgTime <- function(files,nDays="asIn",begin=NULL,end=NULL,pillow=75,pos1=10,pos2=16,format="%Y%j")
{
    if (inherits(files,"Raster"))
    {
      files <- names(files)    
    }
    files <- basename(files)
    
    allDates <- sort(extractDate(files,asDate=TRUE,pos1=pos1,pos2=pos2,format=format)$inputLayerDates)
    
    datLim <- transDate(begin=begin,end=end)
    
    if (!is.null(begin))
    {
      minOUT  <- datLim$begin
      minIN   <- minOUT - pillow
      minHAVE <- min(allDates[allDates >= minIN])
      if (nDays=="asIn")
      {
        minIN <- minHAVE
      }
    } else 
    {
      minIN <- minOUT <- minHAVE <- min(allDates)
    }

    if (!is.null(end))
    {
      maxOUT  <- datLim$end
      maxIN   <- maxOUT + pillow
      maxHAVE <- max(allDates[allDates <= maxIN])
      if (nDays=="asIn")
      {
        maxIN <- maxHAVE
      }
    } else 
    {
      maxIN <- maxOUT <- maxHAVE <- max(allDates)
    }
    
    inputLayerDates <- allDates[allDates >= minHAVE & allDates <= maxHAVE]
    inDoys <- as.numeric(format(as.Date(inputLayerDates),"%j"))
    
    if(FALSE) # currently removed
    {   
      if (minIN < minHAVE)
      {
        if (as.numeric(minHAVE - minIN) <= pillow)
        {
          warning("'begin'-date - 'pillow' is earlier by, ",as.numeric(minHAVE - minIN) ," days, than the available input dates!\nPillow at the start of the time serie is reduced to ",pillow - as.numeric(minHAVE - minIN)," days!")
        } else if (minOUT == minHAVE)
        {      
          warning("Is is not possible to use the pillow at the begin of the time series since there is no data available before 'begin'-date!")
        }
      }
      if (maxIN > maxHAVE)
      {
        warning("'end'-date + 'pillow' is later by, ",as.numeric(maxIN - max(inputLayerDates)) ," days, than the available input dates!")
      }
    }
    if (nDays=="asIn")
    {
      outputLayerDates <- inputLayerDates[datLim$begin <= inputLayerDates & datLim$end > inputLayerDates]
    } else 
    {
      outputLayerDates <- seq(minOUT,maxOUT,by=nDays)
    }
    
    t0     <- as.numeric(min(outputLayerDates,inputLayerDates)) - 1 
    inSeq  <- as.numeric(inputLayerDates) - t0
    outSeq <- as.numeric(outputLayerDates) - t0

    return(list(inSeq=inSeq,outSeq=outSeq, inDoys=inDoys, inputLayerDates=inputLayerDates,outputLayerDates=outputLayerDates,call = list(pos1=pos1,pos2=pos2,format=format,asDate=TRUE,nDays=nDays,pillow=pillow)))        
}




