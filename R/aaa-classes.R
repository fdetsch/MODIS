#' Class MODISextent
#' 
#' @description 
#' An object of class \code{MODISextent}, typically created through 
#' \code{\link{getTile}}.
#'  
#' @slot tile MODIS tile ID as \code{character}.
#' @slot tileH MODIS horizontal tile ID as \code{integer}.
#' @slot tileV MODIS vertical tile ID as \code{integer}.
#' @slot extent \code{Extent} information, see \code{\link{getTile}}.
#' @slot system Sensor system as \code{character}.
#' @slot target If applicable, a \code{list} with additional target information.
#' 
#' @exportClass MODISextent
#' @name MODISextent-class
NULL

setClassUnion("listORnull", c("list", "NULL"))

setClass('MODISextent',
         slots = c(tile = 'character',
                   tileH = 'integer',
                   tileV = 'integer',
                   extent = 'Extent',
                   system = 'character',
                   target = 'listORnull')
         )

NULL

#' Class MODISproduct
#'
#' @description An object of class \code{MODISproduct}, typically created through
#' \code{\link[MODIS]{getProduct}} when the 'x' input is a MODIS product or 
#' regular expression.
#'
#' @slot request User request as \code{character}.
#' @slot PF1,PF2,PF3,PF4 Platform specific path feature for LP DAAC, LAADS, NTSG 
#' and NSIDC as \code{character}.
#' @slot PD Product specific code number following the platform specifier, e.g. 
#' \code{"13A1"} for MOD13A1.
#' @slot PLATFORM Satellite platform on which MODIS sensor is mounted; one of 
#' \code{c("Terra", "Aqua")}.
#' @slot TYPE Product type; one of \code{c("Tile", "CMG", "Swath")}.
#' @slot PRODUCT MODIS product identified from 'request' as \code{character}.
#' @slot SENSOR Statically set to \code{"MODIS"}.
#' @slot SOURCE Product specific MODIS download server(s) as named \code{list}.
#' @slot CCC Product specific MODIS data collection(s) stored as 3-digit 
#' \code{character} objects in a named \code{list}.
#'
#' @exportClass MODISproduct
#' @name MODISproduct-class
NULL

setClass('MODISproduct',
         slots = c(request = 'character', 
                   PF1 = 'character', 
                   PF2 = 'character', 
                   PF3 = 'character', 
                   PF4 = 'character', 
                   PD = 'character',
                   PLATFORM = 'character', 
                   TYPE = 'character', 
                   PRODUCT = 'character', 
                   SENSOR = 'character',
                   SOURCE = 'list', 
                   CCC = 'listORnull')
         )

NULL

#' Class MODISfile
#'
#' @description An object of class \code{MODISfile}, typically created through 
#' \code{\link[MODIS]{getProduct}} when the 'x' input is a MODIS filename.
#'
#' @slot request User request as \code{character}.
#' @slot PRODUCT MODIS product identified from 'request' as \code{character}.
#' @slot DATE Acquisition date string in the form \code{"A\%Y\%j"} (see 
#' \code{\link{strptime}} and 
#' \href{https://modis-images.gsfc.nasa.gov/MOD07_L2/filename.html}{HDF filename convention}).
#' @slot TILE Tile string in the form \code{"hXXvXX"}.
#' @slot CCC MODIS data collection as 3-digit \code{character}.
#' @slot PROCESSINGDATE Processing date string in the form \code{"\%Y\%j\%H\%M\%S"} 
#' (see \code{\link{strptime}}).
#' @slot FORMAT File format as \code{character}.
#' @slot SENSOR Statically set to \code{"MODIS"}.
#' @slot PLATFORM Satellite platform on which MODIS sensor is mounted; one of 
#' \code{c("Terra", "Aqua")}.
#' @slot PF1,PF2,PF3,PF4 Platform specific path feature for LP DAAC, LAADS, NTSG 
#' and NSIDC as \code{character}.
#' @slot TOPIC Product topic as \code{character}.
#' @slot TYPE Product type; one of \code{c("Tile", "CMG", "Swath")}.
#' @slot SOURCE Product specific MODIS download server(s) as named \code{list}.
#' @slot POS1,POS2 Default start and end index of date string in MODIS filename, 
#' usually \code{c("10", "16")}.
#'
#' @exportClass MODISfile
#' @name MODISfile-class
NULL

setClass('MODISfile',
         slots = c(request = 'character', 
                   PRODUCT = 'character',
                   DATE = 'character',
                   TILE = 'character',
                   CCC = 'character',
                   PROCESSINGDATE = 'character', 
                   FORMAT = 'character',
                   SENSOR = 'character',
                   PLATFORM = 'character', 
                   PF1 = 'character', 
                   PF2 = 'character', 
                   PF3 = 'character', 
                   PF4 = 'character', 
                   TYPE = 'character', 
                   SOURCE = 'list', 
                   POS1 = 'integer',
                   POS2 = 'integer'
                   )
)

NULL