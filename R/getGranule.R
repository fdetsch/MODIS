#' Get MODIS Swath Granules
#' 
#' @description 
#' Get MODIS swath granules for a specific geographic area, time period and 
#' (optionally) time of day.
#' 
#' @param product,collection,begin,end,tileH,tileV,extent,... See 
#' \code{\link[MODIS]{getHdf}}.
#' @param DayNightFlag A \code{character} vector of allowed day/night flags.
#' This can be an arbitrary combination of \code{"D"} (day), \code{"N"} (night), 
#' \code{"B"} (both), \code{"X"} (not designated). By default, all flags are 
#' accepted.
#'   
#' @return 
#' Identified granules as \code{character}.
#' 
#' @author Florian Detsch
#' 
#' @seealso \code{\link[MODIS]{getHdf}}, \code{\link[MODIS]{getTile}}.
#' 
#' @examples 
#' \dontrun{
#' data(meuse)
#' pts = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
#'
#' begin <- '2017001'
#' end   <- '2017010'
#'  
#' grn = getGranule("MOD14", begin = begin, end = end
#'                  , extent = pts, DayNightFlag = "D")
#'                  
#' }
#' 
#' @export getGranule
#' @name getGranule
#'
#'  
getGranule = function(product, collection = NULL
                      , begin = NULL, end = NULL
                      , DayNightFlag = c("D", "N", "B", "X")
                      , extent = NULL, ...) {
  
  ### ENVIRONMENT ----
  
  ## set `stringsAsFactors = FALSE`
  saf = getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)
  on.exit(options(stringsAsFactors = saf))
  
  ## suppress download.file() warnings
  w = options("warn")
  options(warn = -1)
  on.exit(options(warn = w))
  
  ## MODISoptions
  opts = combineOptions(...)
  sturheit = stubborn(level = opts$stubbornness)
  
  ## product info  
  prd = getProduct(product, quiet = TRUE)
  
  ## date range
  if (is.null(begin)) {
    begin = ifelse(prd@PLATFORM == "Terra", "2000055", "2002184")
  }
  
  dts = transDate(begin, end)
  dts = seq(dts$begin, dts$end, "day")
  
  ## spatial extent
  if (inherits(extent, "Extent")) {
    extent = as(extent, "SpatialPolygons")
    sp::proj4string(extent) = "+init=epsg:4326"
  } else if (inherits(extent, "sf")) {
    extent = as(extent, "Spatial")
  }
  
  if (!raster::compareCRS(extent, sp::CRS("+init=epsg:4326"))) {
    extent = sp::spTransform(extent, sp::CRS("+init=epsg:4326"))
  }
  
  
  ### METADATA ----
  
  ## loop over years and find relevant .txt files
  idr = file.path("https://ladsweb.modaps.eosdis.nasa.gov/archive/geoMeta"
                  , as.integer(prd@CCC)
                  , toupper(prd@PLATFORM))
  
  # create ifl
  idr
  
  yrs = unique(format(dts,'%Y'))
  mtd = do.call(c, lapply(yrs, function(yr) {
    ifl = file.path(idr, paste0(yr, ".csv"))
    ofl = tempfile(fileext = ".csv")
    
    success = try(log("a"), silent = TRUE); n = 1L
    while (inherits(success, "try-error") & n <= sturheit) {
      success = try(utils::download.file(ifl, ofl, quiet = opts$quiet
                                         , mode = "wb"), silent = TRUE)
      n = n + 1L; Sys.sleep(opts$wait)
    }
    
    onl = utils::read.csv(ofl)
    xtr = extractDate(onl[, 1]
                      , pos1 = 7L, pos2 = 16L
                      , asDate = TRUE, format = "%Y-%m-%d")
    
    ids = xtr$inputLayerDates %in% dts
    file.path(gsub(".csv$", "", ifl), onl[ids, 1])
  }))
  
  
  ### INTERSECTING GRANULES ----
  
  out = vector("list", length(mtd))
  
  for (h in 1:length(mtd)) {
    
    ## download and import daily metadata
    ofl = tempfile(fileext = ".txt")
    
    success = try(log("a"), silent = TRUE); n = 1L
    while (inherits(success, "try-error") & n <= sturheit) {
      success = try(utils::download.file(mtd[h], ofl, quiet = opts$quiet
                                         , mode = "wb"), silent = TRUE)
      n = n + 1L; Sys.sleep(opts$wait)
    }
    
    grn = utils::read.csv(ofl, skip = 1L, header = FALSE)
    
    ## set column names
    lns = readLines(ofl, n = 1L)
    lns = regmatches(lns, regexpr("GranuleID[[:print:]]+", lns))
    names(grn) = strsplit(lns, ",")[[1]]

    ## apply day/night flag
    grn = grn[grn$DayNightFlag %in% DayNightFlag, ]
    
    ## create polygons from bounding box
    crn = sapply(c("West", "East", "South", "North"), function(i) {
      grep(i, names(grn))
    })
    
    pys = do.call(raster::bind, lapply(1:nrow(grn), function(i) {
      crd = unlist(grn[i, crn])
      
      # if granule crosses dateline, create two polygons
      if (crd[2] < crd[1]) {
        crd2.1 = crd2.2 = crd
        
        crd2.1[2] = 180; ext2.1 = raster::extent(crd2.1)
        spy2.1 = as(ext2.1, "SpatialPolygons") 
        crd2.2[1] = -180; ext2.2 = raster::extent(crd2.2)
        spy2.2 = as(ext2.2, "SpatialPolygons")
        
        spy2 = raster::bind(spy2.1, spy2.2)
        spy2 = maptools::unionSpatialPolygons(spy2, IDs = rep(i, length(spy2)))
        
        # else accept as is  
      } else {
        ext2 = raster::extent(crd)
        spy2 = as(ext2, "SpatialPolygons")
      }
      
      sp::proj4string(spy2) = "+init=epsg:4326"
      return(spy2)
    }))
    
    ## find intersecting granules
    sct = suppressWarnings(rgeos::gIntersects(extent, pys, byid = TRUE))
    out[[h]] = grn[apply(sct, 1, FUN = any), "GranuleID"]
  }
  
  names(out) = basename(mtd)
  return(out)
}

