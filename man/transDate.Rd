\name{transDate}

\docType{methods}

\alias{transDate}

\title{Function for MODIS date conversions and testing.}

\description{This function converts input dates (begin and end) to 'YYYY-MM-DD' and 'YYYYDDD'. If no dartes are specified the function uses a past and future date!}

\usage{transDate(begin=NULL,end=NULL)}

\arguments{
  \item{begin}{character string with the following sequences: "YYYY.MM.DD" or "YYYYDDD", separator can be any (".","-","/",...)}
  \item{end}{character string with the following sequences: "YYYY.MM.DD" or "YYYYDDD", separator can be any (".","-","/",...)}
} 

\value{
  character list with 4 slots of class (run the function to see)
}

\author{
  Matteo Mattiuzzi
}

\examples{

transDate()

transDate(begin="2009.01.01")

transDate(end="2009.01.01")

transDate(begin="2009-01-01",end="2011.03.16")
}


