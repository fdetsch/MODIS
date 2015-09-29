\name{smooth.spline.raster}

\docType{methods}

\alias{smooth.spline.raster}

\title{Filter time series imagery with a cubic spline}

\description{
This function uses the \code{smooth.spline} function to filter a vegetation index time serie of satellite data.
}

\usage{
smooth.spline.raster(x,w=NULL,t=NULL,groupYears=TRUE,timeInfo=orgTime(x),
df=6,outDirPath ="./",...)
}

\arguments{
  \item{x}{Sorted 'Vegetation index' raster-Brick or -Stack or filenames.}
  \item{w}{Raster-Brick or -Stack with _weighting_ information. Use \link{makeWeights} for the generation of this file.}
  \item{t}{In case of MODIS composite the 'composite_day_of_the_year' raster-Brick or -Stack or filenames.} 
  \item{groupYears}{Default \code{TRUE}, raster-Brick files separated by years. If \code{FALSE} a overall raster-Brick file.} 
  \item{timeInfo}{result from \code{?orgTime}.} 
  \item{df}{Numeric: _Yearly_ degree of freedom value passed to \code{?smooth.spline}. If set as character (i.e. df="6"), it is not adapted to the time serie length but used as a fixed value (see deteils).} 
  \item{outDirPath}{Output path default is the current directory.}   
  \item{...}{Arguments passed to ?writeRaster, except 'filename', this is created automaticly!}   
} 

\details{
The argument 'df' (degree of freedom) passed to the \code{smooth.spline} function. If you set it as a numeric value (i.e. df=6), you set it as a yearly 'df', this means that it doesn't matter how long the _input_ time serie is because 'df' is adapted to it with: \code{df*('length of _input_ timeserie in days'/365)}. The input length can differ from the output because of the \code{pillow} argument in \code{orgTime}! But you can also set it as a character value (i.e. df="6") in this case the adaption to the time serie length is not performed.  
}

\value{
The filtered data and a text file with the dates of the output layers
}

\note{
Currently tested on MODIS and Landsat data. Using M*D13 data it is also possible to use the 'composite_day_of_the_year' layer and the 'VI_Quality' layer. This function is currently under heavy development and a lot of changes will come!
}

\seealso{
\code{\link{whittaker.raster}}, \code{?raster}
}

\author{
  Matteo Mattiuzzi
}

\examples{
\dontrun{
# The full capacity of the following functions is currently avaliable only with M*D13 data.
# !! The function is very new, double check the result!!

# You need to extract the: 'vegetation index', 'VI_Quality layer', 
# and 'composite day of the year' layer.
# runGdal(product="MOD13A2",begin="2004340",extent="sicily",end="2006070",
# job="fullCapa",SDSstring="101000000010")
# You can download this dataset from (2.7 MB): 
# https://ivfl-rio.boku.ac.at/owncloud/public.php?service=files&t=2fdac3598dba8f5bd865b9dadd715e22&download
# Afterward extract it to: 
options("MODIS_outDirPath")

# the only obligatory dataset is "x" (vegetatino index), get the 'vi' data on the source directory: 
path <- paste0(options("MODIS_outDirPath"),"/fullCapa")
vi <- preStack(path=path, pattern="*_NDVI.tif$")

# "orgTime" detects timing information of the input data and generates based on the arguments
# the output date information. For spline functions (in general) the beginning and
# the end of the time series is always problematic. 
# So there is the argument "pillow" (default 75 days) that adds
# (if available) some more layers on the two endings.
  
timeInfo <- orgTime(vi,nDays=16,begin="2005001",end="2005365",pillow=40)

# now re-run "preStack" with two diferences, 'files' (output of the first 'preStack' call)
# and the 'timeInfo'.
# Here only the data needed for the filtering is extractet:
vi <- preStack(files=vi,timeInfo=timeInfo)

# For speedup try (Sometimes problematic on Win7): 
beginCluster(type="SOCK",exclude="MODIS") # See: ?beginCluster
system.time(smooth.spline.raster(x=vi,timeInfo=timeInfo))

# Filter with weighting and time information:
# if the files are M*D13 you can use also Quality layers and the composite day of the year:
w <- stack(preStack(path=path, pattern="*_VI_Quality.tif$", timeInfo=timeInfo))
w <- makeWeights(w,bitShift=2,bitMask=15,threshold=6)
# you can also pass only the names
t <- preStack(path=path, pattern="*_composite_day_of_the_year.tif$", timeInfo=timeInfo)

beginCluster(type="SOCK",exclude="MODIS") # See: ?beginCluster
system.time(smooth.spline.raster(x=vi,w=w,t=t,timeInfo=timeInfo))

}
}


