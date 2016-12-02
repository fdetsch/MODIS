#' Calculate MODIS Composite Images
#'
#' @description
#' Based on a user-defined function, e.g. \code{max} for maximum value
#' composites (MVC), aggregate native 16-day MODIS datasets to custom temporal
#' composites.
#'
#' @param x \code{Raster*} or \code{character}. MODIS vegetation index.
#' @param y \code{Raster*} or \code{character}. MODIS
#' "composite_day_of_the_year" SDS associated with 'x'.
#' @param interval \code{character}. Time period for aggregation, see
#' \code{\link{aggInterval}}.
#' @param fun \code{function}. See \code{\link{overlay}}.
#' @param cores \code{integer}. Number of cores for parallel processing.
#' @param filename \code{character}. Optional output filename.
#' @param ... Additional arguments passed to \code{\link{writeRaster}}.
#'
#' @return A \code{Raster*} object.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{aggInterval}}, \code{\link{overlay}},
#' \code{\link{writeRaster}}.
#'
#' @examples
#' \dontrun{
#' runGdal("MOD13Q1", collection = getCollection("MOD13Q1", forceCheck = TRUE),
#'         begin = "2015001", end = "2015365", extent = "Luxembourg",
#'         job = "temporalComposite", SDSstring = "100000000010")
#'
#' ndvi <- list.files(paste0(getOption("MODIS_outDirPath"), "/temporalComposite"),
#'                    pattern = "NDVI.tif", full.names = TRUE)
#'
#' cdoy <- list.files(paste0(getOption("MODIS_outDirPath"), "/temporalComposite"),
#'                    pattern = "day_of_the_year.tif", full.names = TRUE)
#'
#' mmvc <- temporalComposite(ndvi, cdoy)
#' plot(mmvc[[1:4]])
#' }
#'
#' @export temporalComposite
#' @name temporalComposite
temporalComposite <- function(x, y,
                              interval = c("month", "fortnight"),
                              fun = function(x) max(x, na.rm = TRUE),
                              cores = 1L, filename = "", ...) {

  if (inherits(x, "character")) x <- raster::stack(x)
  if (inherits(y, "character")) y <- raster::stack(y)

  ## append year to "composite_day_of_the_year"
  y <- MODIS::reformatDOY(y, cores = cores)

  ## create half-monthly time series
  dates_mod <- MODIS::extractDate(x, asDate = TRUE)$inputLayerDates
  dates_seq <- aggInterval(dates_mod, interval[1])

  ## initialize parallel cluster with required variables
  cl <- parallel::makePSOCKcluster(cores)
  parallel::clusterExport(cl, c("x", "y", "fun", "dates_mod", "dates_seq"),
                          envir = environment())

  ## generate temporal composites
  lst_seq <- parallel::parLapply(cl, 1:length(dates_seq$begin), function(i) {
    dff <- dates_mod - dates_seq$begin[i]
    ids <- which(dff <= 16 & dff >= (-16))

    if (length(ids) == 0)
      return(NULL)

    lst <- lapply(ids, function(j) {
      doy <- raster::getValues(raster::subset(y, j))
      out <- which(doy < dates_seq$beginDOY[i] | doy > dates_seq$endDOY[i])

      val <- raster::getValues(raster::subset(x, j))
      val[out] <- NA
      raster::setValues(raster::subset(x, j), val)
    })

    rst <- raster::stack(lst)
    suppressWarnings(rst <- raster::overlay(rst, fun = fun))
    names(rst) <- paste0("A", dates_seq$beginDOY[i])
    return(rst)
  })

  ids <- !sapply(lst_seq, is.null)
  rst_seq <- raster::stack(lst_seq[ids])

  ## deregister parallel backend
  parallel::stopCluster(cl)

  ## write to disk (optional)
  if (nchar(filename) > 0)
    rst_seq <- raster::writeRaster(rst_seq, filename, ...)

  return(rst_seq)
}


