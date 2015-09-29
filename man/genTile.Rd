\name{genTile}

\docType{methods}

\alias{genTile}
\alias{genTile,tileSize,offset}

\title{Generate tiling system}

\description{
This function generates a matrix with bounding box information for a global tiling system (based on Lat/Lon!). 
}

\usage{genTile(tileSize = 1, offset = 0, StartNameFrom = c(0, 0),
 extent = list(xmin = -180, xmax = 180, ymin = -90,ymax = 90))}

\arguments{

  \item{tileSize}{Numeric. Size of a single tile, in degrees (Lat/Lon, WGS84)}

  \item{offset}{Numeric. Shift the tiling system in upper-left direction.}

  \item{StartNameFrom}{Numeric. "c(Lat-Direction,Lon-Direction)" start number in the naming of the tiles.}

  \item{extent}{list. Tile system Extent information (basically the coverage of the data on server).}  

}

\value{Matrix}

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getTile}}
}

\examples{
\dontrun{
e1 <- genTile() # 1x1 degree tiling system.  
# 10x10 degree tiling system with offset to be aligned to Geoland2 Dataset
e2 <- genTile(tileSize=10,offset=(1/112)/2)
# Tiling system for SRTMv4 data (CGIAR-CSI) 
e3 <- genTile(tileSize=5,StartNameFrom = c(1, 1), 
  extent=list(xmin = -180, xmax = 180, ymin = -60,ymax = 60)) 

head(e1)
head(e2)
head(e3)
}
}



