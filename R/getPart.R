# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : February 2012
# Licence GPL v3

################################
# getPart() takes as argument ONLY a defineName() or a getProduct() result, or basicaly a vector with named "nodes"
################################
getPart <- function(x, what = c('YYYY', 'DDD', 'DATE', 'SENSOR', 'PF1', 'PF2', 'PLATFORM', 'TILE', 'TILEV', 'TILEH', 'C', 'CCC', 'PRODUCT', 'FORMAT', 'COMPRESSION', 'DATE1DATE2', 'PROCESSINGDATE', 'REGION', 'TIME'))
{    
    if (missing(x)){
        return(cat("Available 'placeholders' are:",what,"\n",sep=" "))
    }
      
    what <- match.arg(what)
    switch(what,
        YYYY = substring(x$DATE,2,5), # works with AYYYYDDD input # TODO a scanning function to detect teh first numeric value in x$DATE
        DDD  = substring(x$DATE,6,8), # works with AYYYYDDD input
        DATE = gsub(transDate(begin=substring(x$DATE,2,8),)$begin,pattern="-",replacement="."), # works with AYYYYDDD input
        SENSOR = x$SENSOR,
        #PF1 = getProduct(x=x[1],quiet=TRUE)$PF1,
        #PF2 = getProduct(x=x[1],quiet=TRUE)$PF2,
        #PLATFORM = getProduct(x=x[1])$PLATFORM,
        PF1 = x$PF1,
        PF2 = x$PF2,
        PLATFORM = x$PLATFORM,
        TILE = x$TILE,
        C = as.numeric(x$CCC),
        CCC = x$CCC,
        PRODUCT = x$PRODUCT,
        FORMAT = x$FORMAT,
        COMPRESSION = x$COMPRESSION,
        DATE1DATE2 = x$DATE1DATE2,
        PROCESSINGDATE = x$PROCESSINGDATE,
        #REGION = getTile(x$TILE,system="MERIS") # TODO get REGION by Tile
        REGION = "EuropeAfrica", # the only supported for now!
        TIME = x$TIME,
        TILEV = if (sign(x$TILEV)==-1) {paste("s",sprintf("%03d",abs(x$TILEV)))} else {paste("n",sprintf("%03d",x$TILEV))},
        TILEH = x$TILEH
    )
}

