\name{runGdal}

\docType{methods}

\alias{runGdal}

\title{Process MODIS hdf with GDAL}

\description{
Downloads MODIS grid data from archive (FTP or local) and processes the files.}

\usage{runGdal(product, collection=NULL, begin=NULL, end=NULL, extent=NULL, tileH=NULL, 
tileV=NULL, buffer=0, SDSstring=NULL, job=NULL, checkIntegrity=TRUE, wait=0.5,
quiet=FALSE,...)}

\arguments{
  \item{product}{See: \code{?getProduct}}
  \item{collection}{Default is to download most recent collection version. See: \code{?getCollection}}
  \item{begin}{Default is from the beginning of data. See: \code{?transDate}}
  \item{end}{Default is to the end of data. See: \code{?transDate}}
  \item{extent}{Default is NULL for interactive selection. See: \code{?getTile} and Details!}
  \item{tileH}{Numeric. Default is NULL for interactive selection. See: \code{?getTile}}
  \item{tileV}{Numeric. Default is NULL for interactive selection. See: \code{?getTile}}
  \item{buffer}{Numeric. Buffer [in units of the 'outProj'] around the specified extent. See: \code{?getTile}}
  \item{SDSstring}{Default is extract all SDS (layers). See: \code{?getSds}.}
  \item{job}{Character name. Name of the current job for the creation of the output folder. If not specified it is created: 'PRODUCT.COLLECTION_DATETIME'.}
  \item{checkIntegrity}{Default \code{TRUE}. Check if the file is 'healthy' (and download it again if not) before doing further processings with it. This consumes also a littel bit of time to do the check.}
  \item{wait}{Numeric. Time to wait to re-try a un-successful online connection (URL listenting or 'download.file()' process). Keep it small, max ~1 sec.}
  \item{quiet}{Logical, Default \code{FALSE}. Some progress informations.}
  \item{...}{See section 'Methods'.}

} 

\section{Methods}{

\describe{
\tabular{rll}{
\tab \code{outProj}\tab CRS/ prj4 or EPSG code of output, any format supported by gdal see examples.\cr \tab \tab Default is 'asIn' (no warping). See \code{?MODISoptions}.\cr
\tab \code{pixelSize}\tab Numeric single value. Output pixel size in target reference system unit.\cr \tab \tab Default is 'asIn'. See \code{?MODISoptions}.\cr
\tab \code{resamplingType}\tab Character. Default is 'near', can be one of: 'bilinear', 'cubic', 'cubicspline', 'lanczos'.\cr \tab \tab See \code{?MODISoptions}.\cr
\tab \code{blockSize}\tab integer. Default \code{NULL} that means the stripe size is set by GDAL.\cr \tab \tab Basically it is the "-co BLOCKYSIZE=" parameter. See: http://www.gdal.org/frmt_gtiff.html\cr
\tab \code{compression}\tab logical. Default is \code{TRUE}, compress data with the lossless LZW compression with "predictor=2".\cr \tab \tab See: \url{http://www.gdal.org/frmt_gtiff.html}\cr
\tab \code{dataFormat}\tab Data output format, see \code{getOption("MODIS_gdalOutDriver")} column 'name'.\cr
\tab \code{localArcPath}\tab Character.  See \code{?MODISoptions}. Local path to look for and/or to download MODIS files.\cr
\tab \code{outDirPath}\tab Character.  See \code{?MODISoptions}. Root directory where to write \code{job} folder.\cr
}
}
}

\details{
\code{runGdal()} uses a lot of 'MODIS'-package functions, see in section 'arguments' and 'methods' the respective '?function' for details and inputs.\cr
If \code{extent} is a raster* object, the output has exactly the same extent, pixel size and projection as the used raster* object.\cr
If \code{extent} is a spatial* object (ie polygon shapefile), the output has exactly the same extent and projection as the used spatial* object.\cr
If \code{tileH} and \code{tileV} arguments are used (instead of \code{extent}) to define tha area of interest and \code{outProj} amd \code{pixelSize} are \code{'asIn'} the result is only converted from multilayer-HDF to \code{dataFormat}, default "GeoTiff" (\code{?MODISoptions()}).\cr

}

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getHdf}, \link{runMrt}}
}

\note{
You need to have a GDAL installed on your system!\cr
\url{http://www.gdal.org/gdal_utilities.html}\cr\cr
On Unix-alkes install 'gdal-bin' (I.e. Ubuntu: 'sudo apt-get install gdal-bin')\cr
On Windows you need to install GDAL through OSGeo4W (\url{http://trac.osgeo.org/osgeo4w/}) or FWTools (\url{http://fwtools.maptools.org/}) since the standard GDAL does not support HDF4 format  
}

\examples{
\dontrun{
# LST in Austria
runGdal( product="MOD11A1", extent="austria", begin="2010001", end="2010005", SDSstring="101")

# LST with interactiv area selection
runGdal( product="MOD11A1", begin="2010001", end="2010005", SDSstring="101")

### outProj examples
# LST of Austria warped to UTM 34N (the three different possibilites to specify "outProj")
# to find am EPSG or prj4 you may use: prj <- make_EPSG() See  
runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
 SDSstring="101", outProj="EPSG:32634")
 
runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
 SDSstring="101", outProj="32634")
 
runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
 SDSstring="101", outProj="+proj=utm +zone=34 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

### resamplingType examples
runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
 SDSstring="1", resamplingType="lanczos", outProj="32634", pixelSize=100)

### processing entire tiles and keeping Sinusoidal projection
# This corresponds to a format conversion (eos-hdf04 to Geotiff) and 
# layer extraction (multi-layer to single layer)
runGdal( job="LSTaustria", product="MOD11A1", tileH=18:19,tileV=4, begin="2010001", end="2010005",
 SDSstring="1", outProj="asIn")

}
}


