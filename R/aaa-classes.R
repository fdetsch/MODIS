#' Class MODISextent
#' 
#' @description 
#' An object of class \code{MODISextent}, typically created through 
#' \code{\link{getTile}}.
#'  
#' @slot tile MODIS tile ID as \code{character}.
#' @slot tileH MODIS horizontal tile ID as \code{integer}.
#' @slot tileV MODIS vertical tile ID as \code{integer}.
#' @slot extent Extent information, see \code{\link{getTile}}.
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
                   extent = 'ANY',
                   system = 'character',
                   target = 'listORnull')
         )
