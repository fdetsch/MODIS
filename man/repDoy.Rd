\name{repDoy}

\docType{methods}

\alias{repDoy}

\title{'Repair' DOY values in MODIS composites: 'composite_day_of_the_year' layer/SDS}

\description{
Currently works only for MODIS 16 days composites!
In MODIS composites the Julian dates inside SDS/layer 'composite_day_of_the_year' are refereing always to the year they are effectively in. The problem is, that the Layer/SDS name from the last files from AQUA and TERRA within a year can include dates from the following year and so starting again with 1. The problem comes if you want to sort values of a timeserie by date (e.g. for precise time series functions). This function generates a sequential vector beginning always with the earielst SDS/layer date and ending with the total sum of days of the time serie length.}

\usage{repDoy(pixX,layerDate=NULL,bias=0)}

\arguments{
  \item{pixX}{Matrix, normally output of \code{raster:::getValues()}}
  \item{layerDate}{Default \code{NULL}, for autodetection, if you wnat to be sure use the result of \code{extracDate(...,asDate=TRUE)} or \code{orgTime}} 
  \item{bias}{Interger. Bias applied to all values in pixX} 
} 

\value{
Matrix with sequentialised julian dates
}
\author{
  Matteo Mattiuzzi
}

\examples{
# total 112 MB!
\dontrun{
runGdal(product="M.D13A2", begin="2010350", end="2011016", extent="Luxembourg",
job="deleteme", SDSstring="100000000010")

require(raster)

ndviFiles <- preStack(path=paste(MODIS:::.getDef()$outDirPath,"deleteme",sep="/")
,pattern="*_NDVI.tif$")

ndvi <- stack(ndviFiles)

doyFiles <- preStack(path=paste(MODIS:::.getDef()$outDirPath,"deleteme",sep="/")
,pattern="*_composite_day_of_the_year.tif$")

doy <- stack(doyFiles)
layerDates <- extractDate(names(doy))

pixX <- 169

y <- ndvi[pixX]
x1 <- doy[pixX]
x2 <- repDoy(doy[pixX],layerDates)

x1
x2
# the plotting example is not really good. 
# To create a figurative example it would be necessary to dolwnload to much data! 
plot("",xlim=c(1,max(x1,x2)),ylim=c(0,2000),xlab="time",ylab="NDVI*10000")
lines(y=y,x=x1,col="red",lwd=3)
lines(y=y,x=x2,col="green",lwd=2)

# repDoy function is thought to be enbeded in something like that:
tr <- blockSize(ndvi)

doyOk <- brick(doy)
doyOk <- writeStart(doyOk, filename='test.tif',  overwrite=TRUE)

for (i in 1:tr$n)
{
pixX  <- getValues(doy,tr$row[i],tr$nrows[i])
ok    <- repDoy(pixX,layerDates)
doyOk <- writeValues(x=doyOk,v=ok,start=tr$row[i])
}
doyOk <- writeStop(doyOk)

# unlink(filename(doyOk))
}
}

