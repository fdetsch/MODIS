#' MODIS Acquisition and Processing
#'
#' Download and processing functionality for the Moderate Resolution Imaging 
#' Spectroradiometer (MODIS). The package provides automated access to the 
#' global online data archives (LP DAAC and LAADS) and processing capabilities 
#' such as file conversion, mosaicking, subsetting and time series filtering.
#'
#' @name MODIS-package
#' @docType package
#' @title MODIS Acquisition and Processing
#' @author Matteo Mattiuzzi, Florian Detsch
#' \emph{Maintainer:} Florian Detsch \email{fdetsch@@web.de}
#'
#' @import bitops mapdata parallel ptw raster rgdal sf sp
#' @importFrom curl curl curl_download handle_setopt new_handle
#' @importFrom devtools install_github
#' @importFrom grDevices dev.new dev.off png
#' @importFrom graphics abline box grid locator title
#' @importFrom mapedit drawFeatures selectFeatures
#' @importFrom maps map.axes
#' @importFrom maptools map2SpatialPolygons map2SpatialLines checkPolygonsHoles
#' @importFrom methods as new slot
#' @importFrom rgeos gBuffer gIsValid
#' @importFrom stats na.omit setNames smooth.spline
#' @importFrom utils capture.output download.file installed.packages read.csv read.table vi write.csv write.table
#' @rawNamespace if (.Platform$OS.type=="windows") importFrom(utils,shortPathName)
#'
#' @keywords package
#'
NULL
