\name{extractDate}

\docType{methods}

\alias{extractDate}

\title{
Extract dates from (MODIS) files
}

\description{
This function helps to extract dates in a vector of files
}

\usage{
extractDate(files,pos1=10,pos2=16,asDate=FALSE,format='\%Y\%j')
}

\arguments{
\item{files}{vector of filenames where from extract the dates. Can include also the filepath!}
\item{pos1}{Start position of date in the filename. You have to count, sorry!} 
\item{pos2}{End position of date in the filename. You have to count, sorry!}
\item{asDate}{Logical, default is FALSE. If TRUE the result is converted to a \code{Date} object}
\item{format}{Used only if \code{asDate=TRUE}, MODIS dates are normally formatted as "\%Y\%j" (Year and julian day) See: \code{as.Date} for modifications.} 
} 

\value{
  Vector with character, if \code{asDate=FALSE} or vector of "Date"'s if TRUE, extracted from filenames
}

\author{
  Matteo Mattiuzzi
}

\examples{
\dontrun{
# example on HDF files
files <- c("MOD13Q1.A2010209.h18v03.005.2010239071130.hdf",
"MOD13Q1.A2010225.h18v03.005.2010254043849.hdf")
extractDate(files)
extractDate(files,asDate=TRUE)

# on any other file 
files <- c("Myfile_20010101.XXX","Myfile_20010115.XXX","Myfile_20010204.XXX")
extractDate(files,pos1=8,pos2=15)
extractDate(files,pos1=8,pos2=15,asDate=TRUE,format="\%Y\%m\%d")
}
}


