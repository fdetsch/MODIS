\name{MODIS bit encoding}

\docType{methods}

\alias{makeWeights}
\alias{extractBits}
\alias{maskWater}

\title{Extract bit-encoded information from a raster file. Create a weihgts rasterfile}

\description{
This function applies \code{bitMask} and \code{bitShiftR} from package bitops to convert bit-encoded information. It is also possible to convert this information to a scale from 0 to 1 in order to use it as weighting information in functions like \link{whittaker.raster} or \link{smooth.spline.raster}  
}

\usage{
makeWeights(x, bitShift=2, bitMask=15, threshold=NULL, filename='', decodeOnly=FALSE,...)
extractBits(x, bitShift=2, bitMask=15, filename='',...)
maskWater(X, bitShift=NULL, bitMask = NULL, keep = NULL, datatype="INT1U",...)
}

\arguments{
    \item{x}{Matrix, vector or one of Raster-Layer, -Stack or -Brick}
    \item{X}{A raster* object from raster package}  
    \item{bitShift}{bit starting point, see examples and \code{?detectBitInfo}}
    \item{bitMask}{bit mask size, see examples and \code{?detectBitInfo}} 
    \item{threshold}{Threshold for valid quality} 
    \item{filename}{Argument passed to \code{writeRaster}, if not set it writes to a tempfile} 
    \item{decodeOnly}{default FALSE, convert bits to a weights from 0 (not used) to 1 (best quality). Or TRUE only extract selected bits and convert to decimal system.} 
    \item{keep}{If \code{NULL} bits are only encoded, else integer vector of values you want to keep (becomes TRUE), the rest is NA. See examples} 
    \item{datatype}{Argument passed to \code{raster:::writeRaster}}
    \item{...}{Other arguments passed to \code{raster:::writeRaster}} 

} 
\note{
\code{makeWeights} and \code{extractBits} are the identical with the only difference that \code{makeWeights} does additionally convert the data into weighting information.

}

\value{
   A rasterLayer if input is a rasterLayer, a rasterBrick if input is a rasterBrick or rasterStack.
}
\seealso{
\code{detectBitInfo}
}

\author{
  Matteo Mattiuzzi
}

\examples{
\dontrun{

# example MOD13Q1 see https://lpdaac.usgs.gov/products/modis_products_table/mod13q1 
# enter in Layers
# See in TABLE 2: MOD13Q1 VI Quality
# column 1 (bit) row 2 VI usefulness 
bitShift = 2
bitMask = 15 # ('15' is the decimal of the binary '1111')  
# or try to use
detectBitInfo("MOD13Q1") # not all products are available!
viu <- detectBitInfo("MOD13Q1","VI usefulness") # not all products are available!
viu

# simulate bit info
bit <- round(runif(10*10,1,65536))

# extract from vector
makeWeights(bit,bitShift,bitMask,decodeOnly=TRUE)
# the same as
extractBits(bit,bitShift,bitMask)

# create a Raster object
VIqual <- raster(ncol=10,nrow=10)
VIqual[] <- bit

# extract from Raster object
a <- makeWeights(VIqual,bitShift,bitMask,decodeOnly=TRUE)

# linear conversion of 0 (0000) to 15 (1111) to 1 fo 0 
b <- makeWeights(VIqual,bitShift,bitMask,decodeOnly=FALSE)

threshold=6 # every thing < threshold becomes a weight = 0
c <- makeWeights(VIqual,bitShift,bitMask,threshold,decodeOnly=FALSE)

res <- round(cbind(a[],b[],c[]),2)
colnames(res) <- c("ORIG","Weight","WeightThreshold") 
res

#####
# water mask
runGdal(product="MOD13A2",begin="2009001",end="2009001", extent=extent(c(-9,-3 ,54,58)),
  SDSstring="001",outDirPath="~/",job="delme") # 6.4 MB
x <- raster("~/delme/MOD13A2.A2009001.1_km_16_days_VI_Quality.tif")

res1 <- maskWater(x)
plot(res1)

res2 <- maskWater(x,keep=1) # 1 = Land (nothing else)
x11()
plot(res2)

# Land (Nothing else but land) + Ocean coastlines and lake shorelines + shallow inland Water,
# the rest becomes NA
x11()
res3 <- maskWater(x,keep=c(1,2,3)) 
plot(res3)

# unlink("~/delme",recursive=TRUE)

###############

# as on Linux you can read HDF4 directly you can also do:
if(.Platform$OS.type=="unix")
{
  x <- getHdf(HdfName="MOD13A2.A2009001.h17v03.005.2009020044109.hdf", wait=0) # 6.4 MB
  
  detectBitInfo(x) # just info
  getSds(x) # just info
 
  x <- getSds(x)$SDS4gdal[3] # you need 'VI Quality'
  x  <- raster(x) 
  # plot(x)
  # ex <- drawExtent()
  ex <- extent(c(-580779,-200911,5974929,6529959))
  x  <- crop(x,ex) # just for speed-up
 
  res1 <- maskWater(x)
  plot(res1)
 
  res2 <- maskWater(x,keep=1) # 1 = Land (Nothing else but land), the rest becomes NA
  x11()
  plot(res2)
  
  # Land (Nothing else but land) + Ocean coastlines and lake shorelines + shallow inland Water,
  # the rest becomes NA
  res3 <- maskWater(x,keep=c(1,2,3))
  x11()
  plot(res3)
}

}
}

