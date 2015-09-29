\name{getCollection}

\docType{methods}

\alias{getCollection}

\title{Get available collections of MODIS product(s)}

\description{Checks and retirves MODIS collection.}

\usage{getCollection(product, collection=NULL, newest=TRUE, 
forceCheck=FALSE, as="character", quiet=TRUE)}

\arguments{
  \item{product}{Character. MODIS grid product to check for existing collections. For 'MODIS TERRA Vegetation Indices at 250 m ground resolution' use: 'MOD13Q1'. For vegetation indices from TERRA and AQUA use 'MxD13Q1'. See \code{getProduct()} for supported products.}   

  \item{collection}{If provided the function only checks if the collection exists and returns the collection number formatted on base of the \code{as} parameter or a \code{FALSE} in case the collection doesn't exists. The check is performed on the primary dataseouce 'LP DAAC' \url{https://lpdaac.usgs.gov/}.}
    
  \item{newest}{Logical. Default \code{TRUE}, return only the number of the newest collection. or \code{FALSE} all available collections}
 
  \item{forceCheck}{Logical. Default \code{FALSE}, if \code{TRUE} the function connects to the 'LP DAAC'-FTP and gets available collections. Since the collections doesn't change very frequently this check once done it is rarely needed.}

  \item{as}{character. Default \code{'character'}, and returns the typical 3 digits long collection number ie: \code{"005"}. Other possibility is to set it to \code{'numeric'} what would give as a result the numeric \code{5}}

  \item{quiet}{Default \code{TRUE}}
}


\value{
  Vector. With three digits character or a numeric value, indicating the collection version(s). And a file in an hidden folder \code{paste(MODIS:::.getDef('localArcPath'),'/.auxiliaries/collections.txt',sep="")} as database for further calls.
  if \code{collection} is provided only the (formatted) collection is returned if available or a \code{FALSE} if not!
}

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getProduct}}
}

\examples{
\dontrun{

# update or get collections for MOD11C3 and MYD11C3
getCollection(product="M.D11C3")
getCollection(product="M.D11C3",newest=FALSE)

getCollection(product="M.D11C3",collection=3)
getCollection(product="M.D11C3",collection=41)
getCollection(product="M.D11C3",collection="041")
getCollection(product="M.D11C3",forceCheck=TRUE)
}

}
