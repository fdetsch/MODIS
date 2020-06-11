# This file contains default values for the R package 'MODIS'.
# version 0.8.13
# Consult '?MODISoptions' for details and explanations!

#########################
# 1.) Set path for HDF-archive and processing output location. 
# ON WINDOWS ALSO USE SINGLE FORWARD SLASH '/'
# If path does not exist it is created!
# Work also with network share!
# consult '?MODISoptions' for more details
  
# All HDF-data will be (properly) stored in this directory. 
localArcPath <- file.path(gsub("\\\\", "/", tempdir()), 'MODIS_ARC')

# Default output location for MODIS package processing results.
outDirPath   <- file.path(localArcPath, 'PROCESSED')

#########################
# 2.) Download:
# consult '?MODISoptions' for more details
dlmethod     <- 'auto' # Download method passed to ?download.file, 'auto' is always a good choice, if you encouter problems (like 'file not found') switch to 'wget'
stubbornness <- 'high' # How stubborn should MODIS re-try to connect to ftp/http?
wait <- 0.5
quiet <- FALSE

#########################
# 3.) Processing defaults
# It is highly recommended to not modify here, at least not 'resamplingType' as there are several layers that require NN (i.e. VI_Quality, Day of the year,...)!
# consult '?MODISoptions' for more details
  
resamplingType <- 'NN' 
outProj        <- 'asIn'
pixelSize      <- 'asIn'
dataFormat     <- 'GTiff'
  
#########################
# 4) Defaults related to raster package:
# Cellchunk: Comparable with chunksize in ?rasterOption.
# But as no effect was found in adapting chunksize,
# MODIS applies its own variant:minrows <- max(floor(cellchunk/ncol(x)),1) blockSize(x,minrows=minrows).
# On a reasonable working station you can easily increase this to 500000, set 1 for raster defaults
cellchunk <- 1

#########################
# 5.) Set path to GDAL _bin_ directory
# Optional, used to relate writable sf::st_drivers("raster") to file extensions for non-standard formats. 
# Example:
# gdalPath <- 'C:/OSGeo4W/bin'
  
