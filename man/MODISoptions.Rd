\name{MODISoptions}
\alias{MODISoptions}
\title{
Set or retrieve permanent MODIS package options}
\description{
With this function you can get or set _persistant_ MODIS package options (per user or systemwide). Changes here will persist sessions and updates.}
\usage{
MODISoptions(localArcPath, outDirPath, pixelSize, outProj, resamplingType, 
dataFormat, gdalPath, MODISserverOrder, dlmethod, stubbornness,
systemwide = FALSE, quiet = FALSE, save=TRUE, checkPackages=TRUE)
}
\arguments{
  \item{localArcPath}{
File Path where to store downloaded MODIS-HDF files. Default is "~/MODIS_ARC".  
}
  \item{outDirPath}{
The output path where to store results of \code{runGdal} or \code{runMrt}. Default is "~/MODIS_ARC/PROCESSED"
}
  \item{pixelSize}{
Default \code{asIn}. Output pixel size [target reference system units] of processing functions \code{runGdal} and \code{runMrt}.
}
  \item{outProj}{Default \code{asIn}. 
Target reference system where to process data using \code{runGdal} or \code{runMrt}, must be a valid CRS for \code{runGdal}. For \code{runMrt} consult the manual of MRT. Since the two pocessing method do not have common methods, it is suggested to leave the default. See Details.}
  \item{resamplingType}{
Default is \code{NN} (Neares Neightbour). In common for MRT and GDAL are \code{NN', 'CC', 'BIL}. GDAL supports also: \code{cubicspline} and \code{lanczos}, GDAL >= 1.10.0 also \code{mode} and \code{average}.}
  \item{dataFormat}{
Default is \code{GTiff}. Or one of \code{getOption("MODIS_gdalOutDriver")} (column 'name').}

  \item{gdalPath}{
Path to gdal _bin_ directory, relvand more for windows. Use \code{MODIS:::checkTools("GDAL")} to detect it automaticly}
  \item{MODISserverOrder}{Default \code{"LPDAAC","LAADS"}, you may invert, or select only one server, inhibiting all efforts to download data from the second available server.}
  \item{dlmethod}{Default \code{auto} see argument \code{method} in \code{?download.file}. On a Unixoid (also Mac?) it is suggested to use \code{wget} or if installed \code{aria2}.}
  \item{stubbornness}{
The number of re-tries after that the HTTP/FTP server has refused the connection. Higher values increase the chance of getting the file but also leads to hanging functions if the server is down.}
  \item{systemwide}{Default \code{FALSE}. 'User' wide settings saved to \code{path.expand("~/.MODIS_Opts.R")}, if \code{TRUE} write settings to 'systemwide', presumed you have write access to \code{paste(R.home(component="etc"), '/', '.MODIS_opts.R', sep='')}}
  \item{quiet}{Logical. Print or not options to console.}
  \item{save}{Default \code{TRUE} make settings permanent.}
  \item{checkPackages}{Default \code{TRUE} check if suggested \R packages, 'GDAL' and 'MRT' are installed.}

}

\details{
This Settings are _permanent_, easy to change and take effect immediatley!
If you change default values consider that your settings have to be valid for any MODIS product, layer and area!

It is _not_ recommended to use a _not_ globally applicable georeference reference system as default \code{outProj}, or a fixed \code{pixelSize} for different products or a \code{resamplingType} that is not \code{nn}.

\code{localArcPath} and \code{outDirPath} should be changed, expecially on a Windows system as '~/MODIS_ARC/...' is normally on the 'c:/...' drive. You can set also a shared network drive if you have or want a central MODIS data server. 

On Windows you have to set \code{gdalPath} to the location of GDAL executables (the '.../GDAL../bin' directory). On Unix-alikes this should not be required unless you want to specify a non default GDAL installation.

On an unixoid OS it is suggested to use \code{dlmethod='wget'} because its a reliable tool and, after the change of the 'LP DAAC' datapool from FTP to HTTP (May 2013) \code{dlmethod='auto'} seams not to work properly. On Windows it seams to work fine with \code{dlmethod='auto'}. Help and suggestions appreciated! 

}

\author{
Matteo Mattiuzzi and Steven Mosher 
}
\examples{
\dontrun{
MODISoptions()
# MODISoptions(localArcPath="/another/path/than/default")
}
}
