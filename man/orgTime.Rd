\name{orgTime}

\docType{methods}

\alias{orgTime}

\title{Handles the in/output date used in the filerting}

\description{
With this function you define the period to be filtered (begin:end), the output temporal resolution (nDays), and it selected from 'files' the required data.
}

\usage{
  orgTime(files,nDays="asIn",begin=NULL,end=NULL,pillow=75,pos1=10,pos2=16,format="\%Y\%j")
}

\arguments{
  \item{files}{MODIS filenames, eg output of \code{runGdal} or \code{runMrt}.}
  \item{nDays}{Integer. Time interval for the output layers. Default is "asIn" that includes the exact input dates within the period selected using \code{begin} and \code{end}. Can also be nDays="1 month" or "1 week" see: \url{https://www.stat.berkeley.edu/classes/s133/dates.html} example of seq(from,to,by) at the bottom.}
  \item{begin}{Default is from earliest input dataset. Here you can supply the begin date of the output} 
  \item{end}{Default to the end of the input dataset. Here you can specify the end date of the output (note, the exact \code{end}-date depends on \code{start} and \code{nDays} argument.} 
  \item{pillow}{Number of days added on the beginning and on the end of a time serie.} 
  \item{pos1}{Start position of date in the filename. You have to count, sorry!} 
  \item{pos2}{End position of date in the filename. You have to count, sorry!} 
  \item{format}{How is the date formatted in the \code{file}, default expects: 'YYYYDDD' (\code{"\%Y\%j"}). Read \code{?as.Date} for for more information.} 
} 

\value{
  List
}

\author{
  Matteo Mattiuzzi
}

\examples{
\dontrun{
# note, this function can be applied to any files that have a date information in the _filename_!
files <- c("MOD13A2.A2010353.1_km_16_days_composite_day_of_the_year.tif",
 "MOD13A2.A2011001.1_km_16_days_composite_day_of_the_year.tif",
 "MYD13A2.A2010361.1_km_16_days_composite_day_of_the_year.tif",
 "MYD13A2.A2011009.1_km_16_days_composite_day_of_the_year.tif")

orgTime(files)
orgTime(files,nDays=2,begin="2010350",end="2011015")
}
}


