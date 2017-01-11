if ( !isGeneric("orgTime") ) {
  setGeneric("orgTime", function(files, ...)
    standardGeneric("orgTime"))
}
#' Handle Input and Output Dates Used for Filtering
#' 
#' @description 
#' This function lets you define the period to be filtered, the output temporal 
#' resolution, and select the required data from your input 'files'.
#' 
#' @param files A \code{character}, \code{Date}, or \code{Raster*} object. 
#' Typically MODIS filenames created e.g. from \code{\link{runGdal}} or 
#' \code{\link{runMrt}}, but any other filenames holding date information are 
#' supported as well. If a \code{Raster*} object is supplied, make sure to 
#' adjust 'pos1', 'pos2', and 'format' according to its layer 
#' \code{\link[raster]{names}}. 
#' @param nDays Time interval for output layers. Defaults to \code{"asIn"} that 
#' includes the exact input dates within the period selected using \code{begin} 
#' and \code{end}. Can also be \code{nDays = "1 month"} or \code{"1 week"}, see 
#' \code{\link{seq.Date}} and Examples.
#' @param begin \code{character}. Output begin date, defaults to the earliest 
#' input dataset.
#' @param end \code{character}. Output end date, defaults to the latest input 
#' dataset. Note that the exact \code{end} date depends on \code{begin} and 
#' \code{nDays}.
#' @param pillow \code{integer}. Number of days added to the beginning and end 
#' of a time series.
#' @param pos1 \code{integer}, start of date string in \code{files}.
#' @param pos2 \code{integer}, end of date string.
#' @param format \code{character}, see \code{\link{extractDate}}.
#' 
#' @return 
#' A \code{list} with the following slots (to be completed):
#' 
#' \itemize{
#' \item{\code{$inSeq}}
#' \item{\code{$outSeq}}
#' \item{\code{$inDoys}}
#' \item{\code{$inputLayerDates}}
#' \item{\code{$outputLayerDates}}
#' \item{\code{$call}}
#' }
#' 
#' @seealso 
#' \code{\link{seq.Date}}.
#' 
#' @author 
#' Matteo Mattiuzzi, Florian Detsch
#' 
#' @examples 
#' # Using MODIS files
#' files <- c("MOD13A2.A2010353.1_km_16_days_composite_day_of_the_year.tif",
#'            "MOD13A2.A2011001.1_km_16_days_composite_day_of_the_year.tif",
#'            "MYD13A2.A2010361.1_km_16_days_composite_day_of_the_year.tif",
#'            "MYD13A2.A2011009.1_km_16_days_composite_day_of_the_year.tif")
#' 
#' orgTime(files)
#' orgTime(files,nDays=2,begin="2010350",end="2011015")
#' 
#' \dontrun{
#' # Using other files, e.g. from GIMMS (Jul 1981 to Dec 1982)
#' library(gimms)
#' 
#' files.v1 <- system.file("extdata/inventory_ecv1.rds", package = "gimms")
#' files.v1 <- readRDS(files.v1)[1:3]
#' dates.v1 <- monthlyIndices(files.v1, timestamp = TRUE)
#' 
#' orgTime(dates.v1)
#' }
#' 
#' @export orgTime
#' @name orgTime
NULL

################################################################################
### function using 'character' input ###########################################
#' @aliases orgTime,character-method
#' @rdname orgTime
setMethod("orgTime",
          signature(files = "character"),
          function(files,nDays="asIn",begin=NULL,end=NULL,pillow=75,pos1=10,pos2=16,format="%Y%j")
{

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
})


################################################################################
### function using 'Date' input ################################################
#' @aliases orgTime,Date-method
#' @rdname orgTime
setMethod("orgTime",
          signature(files = "Date"),
          function(files, 
                   nDays = "asIn", 
                   begin = NULL, 
                   end = NULL, 
                   pillow = 75) {
            
            ## convert 'Date' to 'character'
            files <- as.character(files)
            
            ## invoke 'character' method
            orgTime(files, nDays, begin, end, pillow, pos1 = 1, pos2 = 10, 
                    format = "%Y-%m-%d")
            })


################################################################################
### function using 'Raster*' input #############################################
#' @aliases orgTime,Raster-method
#' @rdname orgTime
setMethod("orgTime",
          signature(files = "Raster"),
          function(files, 
                   nDays = "asIn", 
                   begin = NULL, 
                   end = NULL, 
                   pillow = 75, 
                   pos1 = 10, 
                   pos2 = 16, 
                   format = "%Y%j") {
            
            ## extract layer names
            files <- names(files)
            
            ## invoke 'character' method
            orgTime(files, nDays, begin, end, pillow, pos1, pos2, format)
          })


