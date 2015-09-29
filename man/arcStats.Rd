\name{arcStats}

\docType{methods}
\alias{arcStats}

\title{Get summarisation of local MODIS hdf data. As Plot or as Table.}

\description{
In the same manner as \code{getHdf()}, this function quantifies the availability of local MODIS hdf data. This function gives you an overview (plot or/and talbe) of locally available MODIS GRID hdf files. 
}

\usage{arcStats(product, collection=NULL, extent="global", begin="2000.01.01",
 end=format(Sys.time(),'\%Y.\%m.\%d'), asMap=TRUE, outName=NULL,...)}

\arguments{

  \item{product}{Character. See \code{?getProduct}. MODIS grid product to be checked. For 'MODIS TERRA Vegetation Indices at 250 m ground resolution' use: \code{MOD13Q1}. For vegetation indices from TERRA and AQUA use \code{M.D13Q1}. Run \code{getProduct()} for an overview of supported products.}   

  \item{collection}{Character or integer. Refers to the MODIS product collection-version. Often '005', but see MODIS pages or run \code{?getCollection} for more information.} 

  \item{extent}{deafult is \code{'global'}. Additionally see the argument \code{'extent'} in \code{?getTile} or for interactive selection use: \code{'extent=getTile()'}.}
      
  \item{begin}{Character. Begin date of MODIS time series. See: \code{?transDate}}

  \item{end}{Character. Default is 'Today' expressed in a function. End date of MODIS time series. See: \code{?transDate}}

  \item{asMap}{\code{TRUE, FALSE or "both"}. Indicates if the output is a 'png' a 'csv' or 'both' of them.}
  
  \item{outName}{Character. Default is the 'product.collection.YYYYMMDDHHMMSS.png/csv' of the function call or if applicable 'product.collection.extent.YYYYMMDDHHMMSS.png/csv'}  

  \item{...}{Any relevant arguments in \code{MODISoptions()}. Most important  \code{'outProj'} and \code{'outDirPath'}}
}

\value{
  An invisible NULL (provably this will change to a matrix like object similar to the '*.csv' output. If \code{'asMap'= TRUE} a 'table.csv' and a 'image.png' file(s) in \code{'outDirPath'} see \code{MODISoptions()}.
}

\author{
  Matteo Mattiuzzi
}


\examples{
\dontrun{
# arcStats result on a webserver:
# "http://ivfl-info.boku.ac.at/index.php/eo-data-processing/status-of-the-local-archive"
#
# The following examples are expecting that you have some data stored locally!
########################################################### 
# generates 2 png's and 2 csv's one for TERRA one for AQUA
arcStats(product="M.D13Q1")

# generates 2 png's and 2 csv's one for TERRA one for AQUA with the specified countries.
arcStats(product="M.D13Q1",extent=c("austria","germany","italy"))

# generates 1 png and 1 csv for AQUA.
arcStats(product="MYD13Q1",begin="2005001",outName="MyDataStart2005")

# generates 1 png for AQUA for the selected area and plots it in 'Sinusoidal'.
arcStats(product="MYD13Q1",begin="2005001",asMap=TRUE, outName="InteractiveSelection2005",
 extent=getTile(), outProj="asIn")

# generates 1 png for AQUA for the selected area and plots it in 'Geographic' Coordinates.
arcStats(product="MYD13Q1",begin="2005001",asMap=TRUE, outName="InteractiveSelection2005",
 extent=getTile(), outProj="GEOGRAPHIC")

}
}


