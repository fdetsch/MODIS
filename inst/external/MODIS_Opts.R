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
localArcPath <- '~/MODIS_ARC'

# Default output location for MODIS package processing results.
outDirPath   <- '~/MODIS_ARC/PROCESSED'

#########################
# 2.) Download:
# consult '?MODISoptions' for more details
dlmethod     <- 'auto' # Download method passed to ?download.file, 'auto' is always a good choice, if you encouter problems (like 'file not found') switch to 'wget'
stubbornness <- 'high' # How stubborn should MODIS re-try to connect to ftp/http?

#########################
# 3.) Processing defaults
# It is highly racommanded to not modify here, at least not 'resamplingType' as there are several layers that require NN (i.e. VI_Quality, Day of the year,...)!
# consult '?MODISoptions' for more details
  
resamplingType <- 'NN' 
outProj        <- 'asIn'
pixelSize      <- 'asIn'
dataFormat     <- 'GTiff'
  
#########################
# 4.) Set path to GDAL _bin_ directory
# More related to Windows, but also to other OS in case of a non standard location of GDAL
# ON WINDOWS install 'OSGeo4W' (recommanded) or 'FWTools', and use SINGLE FORWARD SLASH ('/')!
# consult '?MODISoptions' for more details
# Run: 'MODIS:::.checkTools()' to try to autodetect.
# Example:
# gdalPath <- 'C:/OSGeo4W/bin'
  
