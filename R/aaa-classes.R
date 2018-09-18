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
#' @description An object of class \code{MODISproduct}, typically created as the 
#' \code{\link{invisible}} output of \code{\link[MODIS]{getProduct}}.
#'
#' @slot request User request as \code{character}.
#' @slot PF1,PF2,PF3,PF4 Platform specific path feature for LP DAAC, LAADS, NTSG 
#' and NSIDC as \code{character}.
#' @slot PD Product specific code number as 2-digit \code{character}.
#' @slot PLATFORM Satellite platform on which MODIS sensor is mounted; one of 
#' \code{c("Terra", "Aqua")}.
#' @slot TYPE Product type; one of \code{c("Tile", "CMG", "Swath")}.
#' @slot PRODUCT MODIS product identified from 'request' as \code{character}.
#' @slot SOURCE Product specific MODIS download server(s) as \code{list}.
#' @slot CCC Product specific MODIS data collection as 3-digit \code{character}.
#'
#' @exportClass MODISproduct
#' @name MODISproduct-class
NULL

setClassUnion("charORnull", c("character", "NULL"))

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
                   SOURCE = 'list', 
                   CCC = 'charORnull')
         )

NULL