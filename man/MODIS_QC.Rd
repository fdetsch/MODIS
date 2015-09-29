\name{MODIS QA information}

\docType{methods}

\alias{detectBitInfo}

\title{List MODIS QA (Quality) SDS information: bitShift and bitMask}

\description{
This function returns MODIS QA information for a specific product. It gets the information from a internal database and not all products are available.   
}

\usage{
detectBitInfo(product, what = "all", warn = TRUE)
}

\arguments{
    \item{product}{Modis productname of MODIS filename,  see \code{?getProduct} for possibilities}
    \item{what}{Parameter Name (ie \url{https://lpdaac.usgs.gov/products/modis_products_table/mod13q1}, TABLE 2: MOD13Q1 VI Quality; Long Name)}  
    \item{warn}{Logical}
} 

\value{
   If \code{what="all"} a data.frame, else a list
}

\author{
  Matteo Mattiuzzi
}

\examples{
detectBitInfo("MOD13Q1")
detectBitInfo("MOD13Q1","VI usefulness")

detectBitInfo("MYD17A2")
}

