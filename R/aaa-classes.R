#' Class MODISextent
#' 
#' @description 
#' An object of class `MODISextent`, typically created through [getTile()].
#'  
#' @slot tile MODIS tile ID as `character`.
#' @slot tileH MODIS horizontal tile ID as `integer`.
#' @slot tileV MODIS vertical tile ID as `integer`.
#' @slot extent `Extent` information in [EPSG:4326](https://epsg.io/4326), see 
#'   [getTile()].
#' @slot system Sensor system as `character`.
#' @slot target If applicable, a `list` with additional target information.
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
#' @description
#' An object of class `MODISproduct`, typically created through [getProduct()] 
#' when the 'x' input is a MODIS product or regular expression.
#'
#' @slot request User request as `character`.
#' @slot PF1,PF2,PF3,PF4 Platform specific path feature for LP DAAC, LAADS, NTSG 
#'   and NSIDC as `character`.
#' @slot PD Product specific code number following the platform specifier, e.g. 
#'   `"13A1"` for MOD13A1.
#' @slot PLATFORM Satellite platform on which MODIS sensor is mounted; one of 
#'   `c("Terra", "Aqua")`.
#' @slot TYPE Product type; one of `c("Tile", "CMG", "Swath")`.
#' @slot PRODUCT MODIS product identified from 'request' as `character`.
#' @slot SENSOR Statically set to `"MODIS"`.
#' @slot SOURCE Product specific MODIS download server(s) as named `list`.
#' @slot CCC Product specific MODIS data collection(s) stored as 3-digit 
#'   `character` objects in a named `list`.
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
#' @description
#' An object of class `MODISfile`, typically created through [getProduct()] when
#' the 'x' input is a MODIS filename.
#'
#' @slot request User request as `character`.
#' @slot PRODUCT MODIS product identified from 'request' as `character`.
#' @slot DATE Acquisition date string in the form `"A\%Y\%j"` (see [strptime()] 
#'   and [HDF filename convention](https://modis-images.gsfc.nasa.gov/MOD07_L2/filename.html).
#' @slot TILE Tile string in the form `"hXXvXX"`.
#' @slot CCC MODIS data collection as 3-digit `character`.
#' @slot PROCESSINGDATE Processing date string in the form `"\%Y\%j\%H\%M\%S"` 
#'   (see [strptime()]).
#' @slot FORMAT File format as `character`.
#' @slot SENSOR Statically set to `"MODIS"`.
#' @slot PLATFORM Satellite platform on which MODIS sensor is mounted; one of 
#'   `c("Terra", "Aqua")`.
#' @slot PF1,PF2,PF3,PF4 Platform specific path feature for LP DAAC, LAADS, NTSG 
#'   and NSIDC as `character`.
#' @slot TOPIC Product topic as `character`.
#' @slot TYPE Product type; one of `c("Tile", "CMG", "Swath")`.
#' @slot SOURCE Product specific MODIS download server(s) as named `list`.
#' @slot POS1,POS2 Default start and end index of date string in MODIS filename, 
#'   usually `c("10", "16")`.
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