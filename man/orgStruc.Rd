\name{orgStruc}

\docType{methods}

\alias{orgStruc}

\title{Re-organise MODIS files in the local data archive}

\description{This function organises the storage structure of the MODIS-archive accordingly to the settings in '~/.MODIS_opts.R'. depending on the \code{'source'} you set, you can also use this function to gatter all MODIS-grid files on you Computer and organise it like it is set in '~/.MODIS_opts.R'. The main purpose is the organisation of the archive, but it is also possible to use it for coping a subset of files to a desidered location!}

\usage{orgStruc( from, to, structure, pattern, move=FALSE, quiet=FALSE)}

\arguments{
  \item{from}{Character. Local path to look for MODIS files. If missing the default archive \code{options("MODIS_localArcPath")} is used. Most of the time \code{from} corresponds to \code{localArcPath} but it is not neccessary! I.e. if you want to gather all MODIS data on your Computer home use \code{'~/'}.}

 \item{to}{Character. Default is \code{options("MODIS_localArcPath")}. Found data is copied/moved to this base directory}

 \item{structure}{Character. Default is \code{options("MODIS_arcStructure")}. But you can create your own structure. See examples.}

 \item{pattern}{Regular expression passed to \code{?list.files}. Insert a pattern if you want to extract specific files from your archiv.}

 \item{move}{Logical. Default is \code{TRUE}, this means that files are moved and multiple files are deleted. If \code{FALSE}, files are just copied and remain in origin folder!}
 
  \item{quiet}{Logical. Default is \code{FALSE}}
}

\value{
  If \code{quiet==FALSE} an information of how many files have been moved/copied or deleted.
}

\author{
  Matteo Mattiuzzi
}

\examples{
\dontrun{
# MOVE all MODIS grid data to the directory and structure how defined in the '~/.MODIS_opts.R' file
# (Please check before running: \code{options("MODIS_localArcPath")})
orgStruc()

# COPY all MOD13Q1 from 2001 to folder "~/MyFiles/MOD13Q1.collection/"
orgStruc(pattern="MOD13Q1.A2001*.",to="~/MyFiles",structure="PRODUCT.CCC",move=FALSE)

# COPY all MOD13Q1 to folder "~/MyFiles/"
orgStruc(pattern="MOD13Q1.*.",to="~/MyFiles",structure="",move=FALSE) 
}
}


