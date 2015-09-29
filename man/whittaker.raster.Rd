\name{whittaker.raster}

\docType{methods}

\alias{whittaker.raster}

\title{Filter vegetation index time series imagery with the modified whittaker approach} 

\description{
This function uses a modified whittaker filter function (see references) from package 'ptw' to filter a vegetation index time serie of satellite data.
}

\usage{
whittaker.raster(vi, w=NULL, t=NULL, timeInfo = orgTime(vi), lambda = 5000, nIter= 3, outputAs="single", collapse=FALSE, prefixSuffix=c("MCD","ndvi"), outDirPath=".", outlierThreshold=NULL, mergeDoyFun="max", ...)
}

\arguments{
  \item{vi}{Sorted 'Vegetation index' raster-Brick, Stack or filenames. Use \code{preStack} functionality to ensure the right input.}
  \item{w}{In case of MODIS composite the 'VI_Quality'  raster-Brick, Stack or filenames. Use \code{preStack} functionality to ensure the right input.}
  \item{t}{In case of MODIS composite the 'composite_day_of_the_year'  raster-Brick, Stack or filenames. Use \code{preStack} functionality to ensure the right input. If missing the date is determined using \code{timeInfo}} 
  \item{timeInfo}{output of \code{?orgTime}.} 
  \item{lambda}{_Yearly_ lambda value passed to \code{?ptw:::wit2}.  If set as character (i.e. lambda="600"), it is not adapted to the time serie length but used as a fixed value (see details). High values = stiff/rigid spline} 
  \item{nIter}{Numeric. Number of iteration for the upper envelope fitting.}
  \item{outputAs}{Character. Organisation of output files: \code{single} each date one \code{RasterLayer}, \code{yearly} a \code{RasterBrick} for each year, \code{one} one \code{RasterBrick} for the entire time-serie.}
  \item{collapse}{Logical. Collapse input data of multiple years into on single year before filtering.}
  \item{prefixSuffix}{Filennaming. Naming is composed as dottseparated: \code{paste0(prefixSuffix[1],"YYYDDD",lambda,refixSuffix[2],".defaultFileExtension")}}
  \item{outDirPath}{Output path default is the current directory.}
  \item{removeOutlier}{Logical. See details}  
  \item{outlierThreshold}{numerical in the same unit as \code{vi}, used for outliers removal. See details}
  \item{mergeDoyFun}{Especially when using argument \code{collapse=TRUE}, multiple measurements for one day can be present, here you can choose how those values are merged to one single value: \code{"max"} use the highest value, \code{"mean"} or \code{"weighted.mean"} use the \code{mean} if no weighting \code{"w"} is available and \code{weighted.mean} if it is.}    
  \item{...}{Arguments passed to ?writeRaster (except filename is automatic), NAflag, datatype, overwrite,...}
} 

\details{
The argument 'lambda' is passed to the \code{MODIS:::miwhitatzb1} function. You can set it as a yearly 'lambda', this means that it doesn't matter how long the _input_ time serie is because 'lambda' is adapted to it with: \code{lambda*('length of _input_ timeserie in days'/365)}. The input length can differ from the output because of the \code{pillow} argument in \code{orgTime}! But you can also set it as a character value (i.e. lambda="1000") in this case the adaption to the time serie length is not performed.\cr
If 'removeOutliers=TRUE', a whittaker spline (with the user specified 'lambda') is applied befor the iterative filtering process. If the difference between spline and raw value is bigger than \code{threshold} the raw value is removed. 
}

\value{
The filtered data and a text file with the dates of the output layers
}

\references{
Modified Whittaker smoother, according to Atzberger & Eilers 2011 International Journal of Digital Earth 4(5):365-386.\cr
Implementation in R: Agustin Lobo 2012
}

\note{
Currently tested on MODIS and Landsat data. Using M*D13 it is also possible to use the 'composite_day_of_the_year' layer and the 'VI_Quality' layer. This function is currently under heavy development and a lot of changes will come!
}

\seealso{
\code{\link{smooth.spline.raster}}, \code{?raster}
}

\author{
  Matteo Mattiuzzi and Agustin Lobo
}

\examples{
\dontrun{
# The full capacity of the following functions is currently avaliable only with M*D13 data.
# !! The function is very new, double check the result !!

# You need to extract: 'vegetation index', 'VI_Quality layer', and 'composite day of the year'.
# runGdal(product="MOD13A2",begin="2004340",extent="sicily",end="2006070",
# job="fullCapa",SDSstring="101000000010")
# You can download this dataset from (2.6 MB): 
# https://ivfl-rio.boku.ac.at/owncloud/public.php?service=files&t=2fdac3598dba8f5bd865b9dadd715e22&download
# copy it to: options("MODIS_outDirPath")
# Extract: 
# unzip(paste0(options("MODIS_outDirPath"),"fullCapa.zip"),exdir=options("MODIS_outDirPath"))
# delete the zip file:
# unlink(paste0(options("MODIS_outDirPath"),"fullCapa.zip"))
path <- paste0(options("MODIS_outDirPath"),"fullCapa")

# the only obligatory dataset is the vegetatino index 
# get the 'vi' data in the source directory: 
vi <- preStack(path=path, pattern="*_NDVI.tif$")

# "orgTime" detects timing information of the input data and generates based on the arguments
# the output date information. 
# For spline functions (in general) the beginning and the end of the time series
# is always problematic. So there is the argument "pillow" (default 75 days) that adds
# (if available) some more layers on the two endings.
  
timeInfo <- orgTime(vi,nDays=16,begin="2005001",end="2005365",pillow=40)

# now re-run "preStack" with two differences, 'files' (output of the first 'preStack' call)
# and the 'timeInfo'
# Here only the data needed for the filtering is extracted:
vi <- preStack(files=vi,timeInfo=timeInfo)

# For speedup try (On some Win7 problematic): 
# beginCluster() # See: ?beginCluster
system.time(whittaker.raster(vi,timeInfo=timeInfo,lambda=5000))

# if the files are M*D13 you can use also Quality layers and the composite day of the year:
wt <- preStack(path=path, pattern="*_VI_Quality.tif$", timeInfo=timeInfo)
# can also be already stacked:
inT <- preStack(path=path, pattern="*_composite_day_of_the_year.tif$", timeInfo=timeInfo)

# beginCluster() # See: ?beginCluster 
system.time(whittaker.raster(vi=vi, wt=wt, inT=inT, timeInfo=timeInfo, lambda=5000, overwrite=TRUE))

}
}

