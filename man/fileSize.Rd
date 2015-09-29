\name{file.size}

\docType{methods}

\alias{file.size}

\title{Size in Byte of file(s)}

\description{Function for getting size of any files.}
  
\usage{file.size(file,units="B")}

\arguments{
  \item{file}{Character vector of file(s) with path.}
  \item{units}{Character, default "B" bytes or alternative: "KB", "MB", "GB" or "TB" for kilo Bytes, mega Bytes, giga Bytes and tera Bytes.}} 

\value{
  numeric Vector with the same length of available input files in the unit specified by argument. Directories are excluded! 
}

\author{
  Matteo Mattiuzzi
}

\examples{
\dontrun{
file.size(list.files("./"))
}
}


