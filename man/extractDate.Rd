% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractDate.R
\name{extractDate}
\alias{extractDate}
\title{Extract Dates from (MODIS) Files}
\usage{
extractDate(files, pos1, pos2, asDate = FALSE, format = "\%Y\%j")
}
\arguments{
\item{files}{A \code{character} vector of filenames from which to extract dates.
Alternatively, a \verb{Raster*} with date information in its \code{\link[raster:names]{raster::names()}}.}

\item{pos1, pos2}{Start and end of date string in 'files' as \code{integer}. If
missing, attempts to retrieve positions from a look-up table provided that
'files' comply with the MODIS standard naming convention.}

\item{asDate}{\code{logical}. If \code{TRUE}, the result is converted to a \code{Date}
object.}

\item{format}{\code{character}, date format. Used only if \code{asDate = TRUE}.
Defaults to MODIS date style (i.e., \code{"\\\%Y\\\%j"} for year and Julian day).
See \code{\link[=strptime]{strptime()}} for modifications.}
}
\value{
A \code{list} with the following entries: 'inputLayerDates', 'pos1', 'pos2',
'asDate' and, optionally, 'format'. If \code{asDate = FALSE} (default),
'inputLayerDates' are represented as \code{character}, else as \code{Date}.
}
\description{
This function helps to extract dates from a vector of files.
}
\examples{
# example on HDF files
files <- c("MOD13Q1.A2010209.h18v03.005.2010239071130.hdf",
           "MOD13Q1.A2010225.h18v03.005.2010254043849.hdf")
extractDate(files)
extractDate(files, asDate = TRUE)

# on any other file 
files <- c("Myfile_20010101.XXX", "Myfile_20010115.XXX", "Myfile_20010204.XXX")
extractDate(files, pos1 = 8, pos2 = 15)
extractDate(files, pos1 = 8, pos2 = 15, asDate = TRUE, format = "\%Y\%m\%d")
 
}
\author{
Matteo Mattiuzzi
}
