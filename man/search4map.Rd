\name{search4map}

\docType{methods}

\alias{search4map}

\title{Function for 'mapdata' dearch}

\description{
  This function should simplify the search of a 'mapdata'-based extent}

\usage{search4map(pattern="",database='worldHires',plot=FALSE)}

\arguments{
  \item{pattern}{a regular expression used with \code{?grep}}
  \item{database}{Character. Default id "worldHires", but see \code{?map} for available once.}
  \item{plot}{Logical. Default FALSE, should the function plot you search?}
} 

\value{
  List of length=2. 1: the call to use to generate the given map. 2: The names of the areas within that search
}

\author{
  Matteo Mattiuzzi
}

\seealso{
  \code{\link{getTile},?map,?grep}
}

\examples{
\dontrun{

search4map()

search4map(pattern="USA",plot=TRUE)
search4map(database="state",plot=TRUE)?map

search4map(database="italy",pattern="Bolz",plot=TRUE)

search4map(pattern="Sicily",plot=T)


}
}


