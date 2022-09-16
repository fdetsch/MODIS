#' Reformat MODIS "composite_day_of_the_year" SDS
#' 
#' @description 
#' In order to create custom temporal aggregation levels (e.g., half-monthly, 
#' monthly) from native 16-day MODIS composites, a convenient representation of 
#' the pixel-wise acquisition date is urgently required. Since the MODIS 
#' "composite_day_of_the_year" SDS merely includes the day of the year (DOY), 
#' but not the year itself, this function creates complete date information from
#' both the respective MODIS layer name and the pixel-wise DOY information.
#' 
#' @param x `character` or `Raster*`. MODIS "composite_day_of_the_year" 
#'   layer(s).
#' @param cores `integer`. Number of cores for parallel processing.
#' @param ... Additional arguments passed to [extractDate()].
#' 
#' @return 
#' A `Raster*` object.
#' 
#' @author 
#' Florian Detsch
#' 
#' @seealso 
#' [repDoy()].
#' 
#' @examples 
#' \dontrun{
#' tfs = runGdal("MOD13Q1", collection = "006",
#'               begin = "2000353", end = "2000366", extent = "Luxembourg", 
#'               job = "reformatDOY", SDSstring = "000000000010")
#'         
#' ## raw doy
#' raw <- raster(unlist(tfs))
#' unique(raw[])
#' 
#' ## reformatted dates
#' rfm <- reformatDOY(raw)
#' unique(rfm[])
#' }
#' 
#' @export reformatDOY
#' @name reformatDOY
reformatDOY <- function(x, cores = 1L, ...) {

  ## if 'x' represents filename(s), import as 'Raster*'
  if (inherits(x, "character"))
    x <- raster::stack(x)
    
  ## extract required date information
  dts <- extractDate(x, ...)$inputLayerDates
  yrs <- as.numeric(substr(dts, 1, 4))
  dys <- as.numeric(substr(dts, 5, 7))

  ## initialize parallel cluster and export required objects
  cl <- parallel::makePSOCKcluster(cores)
  on.exit(parallel::stopCluster(cl))
  
  parallel::clusterExport(cl, c("x", "yrs", "dys"), envir = environment())

  ## loop over layers
  rfm <- do.call(
    raster::stack,
    parallel::parLapply(cl, 1:raster::nlayers(x), function(i) {
      # get doy values
      rst <- raster::subset(x, i)
      val <- raster::getValues(rst)


      # if required (i.e., if file date and pixel-based doy differ by more than 
      # 300 days), add +1 to year information of the respective pixel
      yr <- rep(yrs[i], length(val))

      dff <- unique(val) - dys[i]

      if (any(dff < (-300), na.rm = TRUE)) {
        ids <- which(dff < (-300))
        nxt <- unique(val)[ids]

        ids <- which(val %in% nxt)
        yr[ids] <- yr[ids] + 1
      }

      # insert new date values into raster layer
      val <- formatC(val, width = 3, flag = "0")
      val <- suppressWarnings(as.integer(paste0(yr, val)))
      raster::setValues(rst, val)
    })
  )

  ## if length(x) == 1, return 'RasterLayer'
  if (raster::nlayers(rfm) == 1) {
    rfm[[1]]
  ## else return 'RasterStack'  
  } else {
    rfm
  }
}
