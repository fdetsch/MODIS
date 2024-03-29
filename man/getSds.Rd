% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSds.R
\name{getSds}
\alias{getSds}
\title{List SDS Layers in an HDF File}
\usage{
getSds(HdfName, SDSstring = NULL, ...)
}
\arguments{
\item{HdfName}{\code{character}. (Absolute) file name from which to extract SDS
names. Non-existing files are being looked up recursively in
\code{getOption("MODIS_localArcPath")}.}

\item{SDSstring}{An optional \code{character} string of 1s and 0s, see Value.}

\item{...}{Currently not used.}
}
\value{
A \code{list}. If 'SDSstring' is provided, the function reports matching SDS and a
formatted 'SDSstring' (e.g., "1 1 1 0 1"). If omitted, the names of all SDS
in 'HdfName' are returned.
}
\description{
List the names of all scientific data sets (SDS) contained in a specified
MODIS grid HDF file.
}
\examples{
hdf = system.file(
  "external/MOD13A2.A2016145.h18v04.006.2016166145124.hdf"
  , package = "MODIS"
)

getSds(
  hdf
)

getSds(
  hdf
  , SDSstring = 1
)

}
\author{
Matteo Mattiuzzi, Florian Detsch
}
