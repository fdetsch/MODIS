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
#' @param pos1,pos2 \code{integer}. The first (last) element of the date string 
#' in 'x', defaults to the MODIS Land Products naming convention.
#' @param interval \code{character}. Time period for aggregation, see
#' \code{\link{aggInterval}}.
#' @param fun,na.rm \code{function}. See \code{\link{overlay}}.
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
#' tfs <- runGdal("MOD13A1", collection = getCollection("MOD13Q1", forceCheck = TRUE),
#'             begin = "2015001", end = "2015365", extent = "Luxembourg",
#'             job = "temporalComposite", SDSstring = "100000000010")
#' 
#' ndvi <- sapply(tfs[[1]], "[[", 1)
#' cdoy <- sapply(tfs[[1]], "[[", 2)
#'
#' mmvc <- temporalComposite(ndvi, cdoy)
#' plot(mmvc[[1:4]])
#' }
#'
#' @export temporalComposite
#' @name temporalComposite
temporalComposite <- function(x, y, pos1 = 10, pos2 = 16,
                              interval = c("month", "fortnight"),
                              fun = max, na.rm = TRUE,
                              cores = 1L, filename = "", ...) {

  if (inherits(x, "character")) names(x) <- NULL; x <- raster::stack(x)
  if (inherits(y, "character")) names(y) <- NULL; y <- raster::stack(y)

  ## append year to "composite_day_of_the_year"
  y <- reformatDOY(y, cores = cores)

  ## create half-monthly time series
  dates_mod <- extractDate(x, pos1 = pos1, pos2 = pos2, asDate = TRUE)$inputLayerDates
  dates_seq <- aggInterval(dates_mod, interval[1])

  ## initialize parallel cluster with required variables
  cl <- parallel::makePSOCKcluster(cores)
  on.exit(parallel::stopCluster(cl))
  
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
    suppressWarnings(rst <- raster::overlay(rst, fun = fun, na.rm = na.rm))
    names(rst) <- paste0("A", dates_seq$beginDOY[i])
    return(rst)
  })

  ids <- !sapply(lst_seq, is.null)
  rst_seq <- raster::stack(lst_seq[ids])

  ## write to disk (optional)
  if (nchar(filename) > 0)
    rst_seq <- raster::writeRaster(rst_seq, filename, ...)

  return(rst_seq)
}


