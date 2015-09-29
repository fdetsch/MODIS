# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : January 2012
# Licence GPL v3

genTile <- function(tileSize=1,offset=0,StartNameFrom=c(0,0),extent=list(xmin=-180,xmax=180,ymin=-90,ymax=90)) 
{

    # offset is used in case of pixel centrum reference. In such case the offset is res/2
    if (offset!=0) {cat("Warning! Tiles crossing LAT extremas (-90 and +90) are not meaningfull for now! For those tiles the resulting shift in LON is not computed!\n")} 
    
    # set origin in UL
    LON <- seq(extent$xmin,extent$xmax,by=tileSize)
    LON <- LON[-length(LON)]
    LAT <- seq(extent$ymax,extent$ymin,by=-tileSize)
    LAT <- LAT[-length(LAT)]
    
    LON <- LON - offset 
    LAT <- LAT + offset
    
    tiles <- expand.grid(LON,LAT)
    colnames(tiles) <- c("xmin","ymax")
    
    iv <- (0:(length(LON)-1)) + StartNameFrom[2]
    ih <- (0:(length(LAT)-1)) + StartNameFrom[1]
    
    vh <- expand.grid(iv,ih)
    tiles$iv <- vh[,2]
    tiles$ih <- vh[,1]
    
    tiles$xmax <- tiles$xmin + tileSize 
    tiles$ymin <- tiles$ymax - tileSize
    
    tiles[tiles$xmin < -180,"xmin"] <- tiles[tiles$xmin < -180,"xmin"] + 2*180
    tiles[tiles$xmin >  180,"xmin"] <- tiles[tiles$xmin >  180,"xmin"] - 2*180
    tiles[tiles$xmax < -180,"xmax"] <- tiles[tiles$xmax < -180,"xmax"] + 2*180 
    tiles[tiles$xmax >  180,"xmax"] <- tiles[tiles$xmax >  180,"xmax"] - 2*180
    
    #TODO: EXTREMAS IN LAT are changing LON +- 180
    # tiles[tiles[,"lat"] < -90,"lon"] <- tiles[tiles[,"lat"] < -90,"lon"] + 180 #
    tiles[tiles$ymax < -90,"ymax"] <- -90 + abs(90 + tiles[tiles$ymax < -90,"ymax"]) 
    tiles[tiles$ymax >  90,"ymax"] <- (2*90) - tiles[tiles$ymax >  90,"ymax"]
    tiles[tiles$ymin < -90,"ymin"] <- -90 + abs(90 + tiles[tiles$ymin < -90,"ymin"]) 
    tiles[tiles$ymin >  90,"ymin"] <- (2*90) - tiles[tiles$ymin >  90,"ymin"]
    
    tiles <- tiles[,c(3,4,1,2,5,6)]
    return(tiles)
}

