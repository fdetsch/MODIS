\name{delHdf}

\docType{methods}
\alias{delHdf}

\title{Delete MODIS grid files to reduce the local storage}

\description{
Delete MODIS grid files to reduce the local storage.
}

\usage{delHdf(product, collection=NULL, extent="global", tileV=NULL, tileH=NULL, begin=NULL, 
end=NULL, ask=TRUE,...)}

\arguments{

  \item{product}{Character. MODIS grid product to be deleted. For 'MODIS TERRA Vegetation Indices at 250 m ground resolution' use: \code{MOD13Q1}. For vegetation indices from TERRA and AQUA use \code{M.D13Q1}. Run \code{getProduct()} for an overview of supported products.}   

  \item{collection}{Default is the newest available onlie, or character or integer. Refers to the MODIS product collection-version. Often '005', but see MODIS pages or run \code{?getCollection} for more information.} 

  \item{extent}{Default "global", that meant all available tiles. for any other settings than "global" see \code{'extent'} in \code{?getTile}.}
    
  \item{tileV}{Default NULL, numeric or character vector. Vertical tile number(s) (for MODIS between 0 and 17). I.e.: \code{tileV = 9:17}. If provided \code{extent} is ignored.}

  \item{tileH}{Default NULL, numeric or character vector. Horizontal tile number(s) (for MODIS between 0 and 35). I.e.: \code{tileH = 1:5}. Refer to \url{http://modis-land.gsfc.nasa.gov/MODLAND_grid.htm} to find your area. If provided \code{extent} is ignored.}

  \item{begin}{Start date,default is from the beginning. Every file \code{'>='} than this date up to \code{end} will be deleted  See: \code{?transDate} for formatting.}

  \item{end}{End date, default is to the most recent. Every file \code{'<='} than this date down to \code{begin} will be deleted  See: \code{?transDate} for formatting.}

  \item{ask}{Default \code{TRUE}. Should the function, after the checking ask for deletion?}
\item{...}{Arguments from \code{?MODISoptions}, ie localArcDir}
}

\value{
  Numeric, the amount of mega Bytes removed from archive, if files have been found, else \code{NULL} if no files have been found.
}

\author{
  Matteo Mattiuzzi
}

\examples{
\dontrun{

# YOU ARE ASKED TO CONFIM THE DELETION! BUT IF THE EXAMPLE THIS FOR YOU SENSITIVE DATA CHANGE IT!

# REMOVE "MYD11A2" from specific date range and area subset:
# delHdf(product="MYD11A2",begin="2010001",end="2010.02.01",extent="austria")
# or
# delHdf(product="MYD11A2",begin="2010001",end="2010.02.01",tileV=18:19,tileH=4)

# REMOVE "MOD11A2" and "MYD11A2" from specific date range but globaly:
# delHdf(product="M.D11A2",begin="2010001",end="2010.02.01") 

# REMOVE ALL "MOD11A2" from local archive:
# delHdf(product="MOD11A2") 
 
}
}


