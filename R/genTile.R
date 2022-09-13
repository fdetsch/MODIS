#' Generate Global Tiling System
#' 
#' @description 
#' This function generates a matrix with bounding box information for a global 
#' tiling system (based on Lat/Lon). 
#' 
#' @param tileSize `numeric`, size of a single tile in degrees (EPSG:4326).
#' @param offset `numeric`, shifts the tiling system in upper-left direction.
#' @param StartNameFrom `numeric`. `c(Lat-Direction,Lon-Direction)` start number
#'   in the naming of the tiles.
#' @param extent `list`. Tile system extent information, basically the coverage 
#'   of the data on server.
#' 
#' @return 
#' A `matrix`.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @seealso 
#' [getTile()].
#' 
#' @examples 
#' # 1x1 degree tiling system
#' e1 <- genTile()  
#' head(e1)
#'   
#' # 10x10 degree tiling system with offset to be aligned to Geoland2 Dataset
#' e2 <- genTile(tileSize = 10, offset = (1/112) / 2)
#' head(e2)
#' 
#' # Tiling system for SRTMv4 data (CGIAR-CSI) 
#' e3 <- genTile(tileSize = 5, StartNameFrom = c(1, 1), 
#'               extent = list(xmin = -180, xmax = 180, ymin = -60,ymax = 60)) 
#' head(e3)               
#' 
#' @export genTile
#' @name genTile
genTile <- function(tileSize = 1, offset = 0, StartNameFrom = c(0, 0),
                    extent = list(xmin = -180, xmax = 180, ymin = -90, ymax = 90)) 
{

    # offset is used in case of pixel centrum reference. In such case the offset is res/2
    if (offset!=0) {cat("Warning! Tiles crossing LAT extremas (-90 and +90) are not meaningful for now! For those tiles the resulting shift in LON is not computed!\n")} 
    
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

