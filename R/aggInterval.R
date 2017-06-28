#' Create Periods for Temporal Composites
#'
#' @description
#' The creation of custom temporal aggregation levels (e.g., half-monthly,
#' monthly) from native 16-day MODIS composites usually requires the definition
#' of date sequences based on which the "composite_day_of_the_year" SDS is
#' further processed. Complementing \code{\link{transDate}}, which returns the
#' respective start and end date only, this function creates full-year
#' (half-)monthly or annual composite periods from a user-defined temporal range.
#'
#' @param x \code{Date} object, see eg default value of 'timeInfo' in 
#' \code{\link{temporalComposite}}.
#' @param interval \code{character}. Time period for aggregation. Currently
#' available options are "month" (default), "year" and "fortnight" (i.e., every 
#' 1st and 15th day of the month).
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
#' dates <- do.call("c", lapply(2015:2016, function(i) {
#'   start <- as.Date(paste0(i, "-01-01"))
#'   end <- as.Date(paste0(i, "-12-31"))
#'   seq(start, end, 16)
#' }))
#' 
#' intervals <- c("month", "year", "fortnight")
#' lst <- lapply(intervals, function(i) {
#'   aggInterval(dates, interval = i)
#' }); names(lst) <- intervals
#' 
#' print(lst)
#'
#' @export aggInterval
#' @name aggInterval
aggInterval <- function(x, interval = c("month", "year", "fortnight")) {
  
  ## date range
  rng <- c(min(x), max(x))
  x <- as.numeric(strftime(x, "%Y"))
  

  ### monthly or fortnightly aggregation -----  
  
  if (interval[1] != "year") {
    
    ## create start date sequence
    st <- lapply(min(x):max(x), function(i) {
      do.call(c, lapply(formatC(1:12, width = 2, flag = "0"), function(j) {
        as.Date(paste(i, j, if (interval[1] == "month") "01" else c("01", "15"),
                      sep = "-"))
      }))
    })

    ## limit start date range to input period    
    st <- do.call(c, st)
    bfr <- st < rng[1]; afr <- st > rng[2]
    st <- if (all(any(bfr), any(afr))) {
      st[which(bfr)[length(which(bfr))]:(which(afr)[1] - 1)]
    } else if (any(bfr) & all(!afr)) {
      st[which(bfr)[length(which(bfr))]:length(st)]
    } else if (all(!bfr) & any(afr)) {
      st[1:(which(afr)[1] - 1)]
    } else {
      st
    }

    
    ## create end date sequence
    nd <- lapply(1:length(st), function(i) {
      if (i < length(st)) {
        st[i + 1] - 1
      } else {
        if (interval[1] == "fortnight" & substr(st[i], 9, 10) == "01") {
          st[i] + 13
        } else {
          mn <- as.integer(strftime(st[i], "%m"))
          dec <- mn + 1 == 13
          
          if (dec) {
            yr <- as.integer(substr(st[i], 1, 4))
            nx <- paste0(yr + 1, "-01-")
            as.Date(gsub(substr(st[i], 1, 8), nx, st[i])) - 1
          } else {
            nx <- paste0("-", formatC(mn + 1, width = 2L, flag = "0"), "-")
            as.Date(gsub(substr(st[i], 5, 8), nx, st[i])) - 1
          }
        }
      }
    })
    
    nd <- do.call(c, nd)

    
  ### annual aggregation -----
    
  } else {
    st <- as.Date(paste0(min(x):max(x), "-01-01"))
    nd <- as.Date(paste0(min(x):max(x), "-12-31"))
  }

  st_doy <- transDate(st)$beginDOY
  nd_doy <- suppressWarnings(transDate(nd)$beginDOY)
  
  ## return named list
  list(begin = st, end = nd,
       beginDOY = st_doy, endDOY = nd_doy)
}
  