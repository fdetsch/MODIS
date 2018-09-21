# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : February 2012

################################
# getPart() takes as argument ONLY a defineName() or a getProduct() result, or basicaly a vector with named "nodes"
################################
getPart <- function(x, what = c('YYYY', 'DDD', 'DATE', 'SENSOR', 'PF1', 'PF2'
                                , 'PF3', 'PF4', 'PLATFORM', 'TILE', 'TILEV', 'TILEH'
                                , 'C', 'CCC', 'PRODUCT', 'FORMAT', 'COMPRESSION'
                                , 'DATE1DATE2', 'PROCESSINGDATE', 'REGION'
                                , 'TIME'))
{    
    if (missing(x)){
        return(cat("Available 'placeholders' are:",what,"\n",sep=" "))
    }
      
    what <- match.arg(what)
    switch(what,
        YYYY = substring(x@DATE,2,5), # works with AYYYYDDD input # TODO a scanning function to detect teh first numeric value in date
        DDD  = substring(x@DATE,6,8), # works with AYYYYDDD input
        DATE = gsub(transDate(begin=substring(x@DATE,2,8))$begin,pattern="-",replacement="."), # works with AYYYYDDD input
        PF1 = x@PF1,
        PF2 = x@PF2,
        PF3 = x@PF3,
        PF4 = x@PF4,
        PLATFORM = x@PLATFORM,
        TILE = x@TILE,
        C = as.numeric(x@CCC),
        CCC = x@CCC,
        PRODUCT = x@PRODUCT,
        SENSOR = x@SENSOR,
        FORMAT = x@FORMAT,
        PROCESSINGDATE = x@PROCESSINGDATE,
    )
}

