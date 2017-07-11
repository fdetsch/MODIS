#' MODIS Acquisition and Processing
#'
#' Download and processing functionalities for the Moderate Resolution Imaging 
#' Spectroradiometer (MODIS). The package provides automated access to the 
#' global online data archives (LPDAAC and LAADS) and processing capabilities 
#' such as file conversion, mosaicking, subsetting and time series filtering.
#'
#' @name MODIS-package
#' @docType package
#' @title MODIS Acquisition and Processing
#' @author Matteo Mattiuzzi, Jan Verbesselt, Tomislav Hengl, Anja Klisch, 
#' Forrest Stevens, Steven Mosher, Bradley Evans, Agustin Lobo, Florian Detsch 
#' \cr
#' \cr
#' \emph{Maintainer:} Matteo Mattiuzzi \email{matteo@@mattiuzzi.com}
#'
#' @import bitops mapdata parallel ptw raster RCurl rgdal sp XML
#' @importFrom devtools install_github
#' @importFrom grDevices dev.new dev.off png
#' @importFrom graphics abline box grid locator title
#' @importFrom mapedit selectFeatures
#' @importFrom maps map.axes
#' @importFrom methods as
#' @importFrom sf st_as_sf
#' @importFrom stats na.omit smooth.spline
#' @importFrom utils capture.output download.file installed.packages read.table vi write.csv write.table
#' @rawNamespace if (.Platform$OS.type=="windows") importFrom(utils,shortPathName)
#'
#' @keywords package
#'
NULL
