#' Create (Half-)Monthly Composite Periods
#'
#' @description
#' The creation of custom temporal aggregation levels (e.g., half-monthly,
#' monthly) from native 16-day MODIS composites usually requires the definition
#' of date sequences based on which the "composite_day_of_the_year" SDS is
#' further processed. Complementing \code{\link{transDate}}, which returns the
#' respective start and end date only, this function creates full-year
#' (half-)monthly composite periods from a user-defined temporal range.
#'
#' @param x \code{numeric} year or \code{Date} object.
#' @param interval \code{character}. Time period for aggregation. Currently
#' available options are "month" (default) and "fortnight" (i.e., every 1st and
#' 15th day of the month).
#'
#' @return
#' A \code{list} with the following slots:
#'
#' \itemize{
#' \item{\code{$begin}: The start date(s) of each (half-)monthly timestep as
#' \code{Date} object.}
#' \item{\code{$end}: Same for end date(s).}
#' \item{\code{$beginDOY}: Similar to \code{$begin}, but with \code{character}
#' objects in MODIS-style date format (i.e., "\%Y\%j"; see \code{\link{strptime}}).}
#' \item{\code{$endDOY}: Same for end date(s).}
#' }
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{transDate}}.
#'
#' @examples
#' dates <- c(2015, 2016)
#'
#' aggInterval(dates)
#' aggInterval(dates, interval = "fortnight")
#'
#' @export aggInterval
#' @name aggInterval
aggInterval <- function(x, interval = c("month", "fortnight")) {

  ## if 'Date' is specified, convert to 'numeric'
  if (inherits(x, "Date"))
    x <- as.numeric(strftime(x, "%Y"))

  ## create start date sequence
  st <- lapply(min(x):max(x), function(i) {
    do.call(c, lapply(formatC(1:12, width = 2, flag = "0"), function(j) {
      as.Date(paste(i, j, if (interval[1] == "month") "01" else c("01", "15"),
                    sep = "-"))
    }))
  })

  st <- do.call(c, st)
  st_doy <- suppressWarnings(MODIS::transDate(st)$beginDOY)


  ## create end date sequence
  nd <- lapply(1:length(st), function(i) {
    if (i < length(st)) {
      st[i + 1] - 1
    } else {
      st[i] + ifelse(interval[1] == "month", 30, 16)
    }
  })

  nd <- do.call(c, nd)
  nd_doy <- suppressWarnings(MODIS::transDate(nd)$beginDOY)

  ## return named list
  list(begin = st, end = nd,
       beginDOY = st_doy, endDOY = nd_doy)
}
