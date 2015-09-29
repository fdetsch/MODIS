\name{getProduct}

\docType{methods}

\alias{getProduct}

\title{Check the avalability of a given product, create file related information}

\description{
On user side it is a funtion to find the desidered product. On package site it generates central internal information to hande files.  
}

\usage{getProduct(x,quiet=FALSE)}

\arguments{

  \item{x}{Character. MODIS filename, MODIS Product name or Regular expression (for regular expression argument see in \code{?grep} the argument \code{pattern} for details).}   

  \item{quiet}{Logical. Default \code{FALSE}.}

}

\value{
An invisible list with usable information for other functions.
}

\author{
  Matteo Mattiuzzi
}

\examples{

getProduct() # list available products

getProduct("MxD11C3") # 'x' for backward compatibility
getProduct("MxD11C3",quiet=TRUE)
# or
getProduct("M.D11C3") # regular expression style
getProduct("M.D11C") # regular expression style

getProduct("MOD14$") # regular expression style
getProduct("MOD14") # regular expression style

internal_info <- getProduct("MOD11C3",quiet=TRUE) # regular expression style
internal_info

getProduct("SRTM") # Only download

}
