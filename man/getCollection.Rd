% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCollection.R
\name{getCollection}
\alias{getCollection}
\title{Get Available Collections of MODIS Product(s)}
\usage{
getCollection(
  product,
  collection = NULL,
  newest = TRUE,
  forceCheck = FALSE,
  as = "character",
  ...
)
}
\arguments{
\item{product}{\code{character}. MODIS grid product to check for existing
collections, see \code{\link[=getProduct]{getProduct()}}.}

\item{collection}{\code{character} or \code{integer}. If provided, the function only
checks if the specified collection exists and returns the collection number
formatted based on the 'as' parameter or \code{FALSE} if it doesn't exists. The
check is performed on \href{https://lpdaac.usgs.gov/}{LP DAAC} as the exclusive
source for several products or, for snow cover (MOD/MYD10) and sea ice
extent (MOD/MYD29), \href{https://nsidc.org/home}{NSIDC}.}

\item{newest}{\code{logical}. If \code{TRUE} (default), return only the newest
collection, else return all available collections.}

\item{forceCheck}{\code{logical}, defaults to \code{FALSE}. If \code{TRUE}, connect to the
LP DAAC or NSIDC server and get available collections, of which an updated
version is permanently stored in \code{MODIS:::combineOptions()$auxPath}.}

\item{as}{\code{character}, defaults to \code{"character"} which returns the typical
3-digit collection number (i.e., \code{"061"}). \code{as = "numeric"} returns the
result as \code{numeric} (i.e., \code{61}).}

\item{...}{Additional arguments passed to \code{\link[=MODISoptions]{MODISoptions()}}. Permanent
settings for these arguments are temporarily overridden.}
}
\value{
A 3-digit \code{character} or \code{numeric} object (depending on 'as') or, if
\code{length(product) > 1}, a \code{list} of such objects with each slot corresponding
to the collection available for a certain product. Additionally, a text file
in a hidden folder located in \code{getOption("MODIS_localArcPath")} as database
for future calls. If 'collection' is provided, only the (formatted)
collection (or \code{FALSE} if it could not be found) is returned.
}
\description{
Checks and retrieves available MODIS collection(s) for a given product.
}
\examples{
\dontrun{

# update or get collections for MCD12C1 and MCD12Q1
getCollection(product = "MCD12.*")
getCollection(product = "MCD12.*", newest = FALSE)
getCollection(product = "MCD12.*", forceCheck = TRUE)
}

}
\seealso{
\code{\link[=getProduct]{getProduct()}}.
}
\author{
Matteo Mattiuzzi, Florian Detsch
}
