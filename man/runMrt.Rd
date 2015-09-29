\name{runMrt}

\docType{methods}

\alias{runMrt}

\title{Run Modis Reprojection Tool ('MRT-grid') with specified parameters}

\description{
Specifying input parameters this function, gets MODIS grid data from archive (HTTP/FTP or local) and processes the files with the 'MRT-grid' tool.}

\usage{runMrt(...)}

\arguments{
  \item{...}{See: Methods}
} 

\section{Methods}{

\describe{
\tabular{rll}{
\tab \code{product}\tab See: \code{?getProduct}\cr
\tab \code{begin}\tab See: \code{?transDate}\cr
\tab \code{end}\tab See: \code{?transDate}\cr
\tab \code{extent}\tab See: \code{?getTile} and Details!\cr
\tab \code{SDSstring}\tab See: \code{?getSds. Default is extract all SDS (layers).}\cr
\tab \code{job}\tab Character. Name of the current job for the creation of the output folder. If not specified it is created: 'PRODUCT.COLLECTION_DATETIME'\cr

\tab \code{localArcPath}\tab Character. Default \code{options("MODIS_localArcPath")}. Local path to look for and/or to download MODIS files.\cr
\tab \code{outDirPath}\tab Character. Default \code{options("MODIS_outDirPath")}. Root directory where to write \code{job} folder.\cr

\tab \code{dataType}\tab Default 'GeoTiff' specified in \code{MODISoptions()}, 'MRT' supports: \code{raw binary} (hdr+dat), \code{HDF-EOS} (hdf) or \code{GeoTiff} (tif). Any other format specified through \code{MODISoptions} or \code{dataType}, is switched to 'GeoTiff'.\cr

\tab \code{outProj}\tab Character. See 'MRT' manual.\cr
\tab \code{zone}\tab Zone number used as optional setting only for \code{outProj="UTM"}, if not set it is autodetected. See 'MRT' manual.\cr
\tab \code{projPara}\tab Character. In that form: "6371007.18 0.00 0.00 ...". for \code{outProj \%in\% c("GEO","SIN")} it is autodetected. See 'MRT' manual.\cr
\tab \code{datum}\tab Character. Default 'NODATUM'. See 'MRT' manual.\cr

\tab \code{mosaic}\tab Logical, default \code{TRUE}. Mosaic files of not? One case for setting \code{mosaic=FALSE} is a to large \code{extent}. HDF4 file supports max 2GB filesize, if crossed mosaicing process will fail.\cr
\tab \code{anonym}\tab Logical, default \code{TRUE}. If \code{FALSE} the job name is added at the end of the root filename.\cr
\tab \code{quiet}\tab Logical. Default \code{FALSE}, its up to you to switch to 'boring' alias \code{FALSE}. Not fully implemented!\cr
\tab \code{dlmethod}\tab default \code{options("MODIS_dlmethod")}. Argument passed to \code{?download.file}. . See \code{?MODISoptions}\cr
\tab \code{stubbornness}\tab Default \code{options("MODIS_stubborness")}. See \code{?MODISoptions}\cr
}
}
}
\author{
  Matteo Mattiuzzi and Forrest Stevens
}

\seealso{
  \code{\link{getHdf}}
}

\source{
You can obtain MRT-grid after registration from: \url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool}
}

\examples{
\dontrun{

runMrt( product="MOD11A1", extent="austria", begin="2010001", end="2010002", SDSstring="101",
  job="ExampleGEOdelme", outProj="GEO")
runMrt( product="MOD11A1", extent="austria", begin="2010001", end="2010002", SDSstring="101",
  job="ExampleSINdelme", outProj="SIN")
runMrt( product="MOD11A1", extent="austria", begin="2010001", end="2010002", SDSstring="101",
  job="ExampleUTMdelme", outProj="UTM")

}
}


