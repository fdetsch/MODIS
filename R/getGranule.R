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
#' grn = getGranule("MOD14", begin = "2000360", end = "2001003"
#'                  , extent = pts, DayNightFlag = "D")
#' }
#' 
#' @export getGranule
#' @name getGranule
getGranule = function(product, collection = NULL
                      , begin = NULL, end = NULL
                      , DayNightFlag = c("D", "N", "B", "X")
                      , extent = NULL, ...) {
  
  ### ENVIRONMENT ----
  
  ## set `stringsAsFactors = FALSE`
  saf = getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)
  on.exit(options(stringsAsFactors = saf))
  
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


listGeometa = function(platform=c('terra', 'aqua'),
                       begin = NULL, end = Sys.Date(), 
                       collection=6, forceRefresh=FALSE, ...) # 
  {
  
  opts <- MODIS:::combineOptions()
  url <- 'https://ladsweb.modaps.eosdis.nasa.gov/archive/geoMeta/'
  

  collection <- as.integer(collection)
  stopifnot(collection %in% 5:6)
  
  platform <- toupper(platform)
  stopifnot(length(platform) == 1)
  stopifnot(tolower(platform) %in% c('terra', 'aqua'))
  
  dts <- transDate(begin, end)
  
  ## date range
  if(platform=='TERRA')
  {
    begin <- max("2000055",dts$beginDOY) 
  } else
  {
    begin <- max("2002184",dts$beginDOY)
  }
  dts <- transDate(begin, end)
  
  dates   <- seq(dts$begin,dts$end,by='day')
  years   <- unique(format(dates,'%Y'))
  basedir <- paste0(opts$auxPath,"geoMeta/",collection,"/",platform,"/")
  csvDir  <- paste0(basedir,"csv/")  
  dir.create(csvDir,showWarnings = FALSE,recursive = TRUE)
  
  # querry folder content from CSV 
  ##    
  # create urls
  rcsv <- paste0(url,collection,"/",platform,"/",unique(format(dates,'%Y')),".csv") 
  lcsv <- paste0(csvDir,years,".csv")
  
  if(forceRefresh)
  {
    needed <- rep(TRUE, length(lcsv))  
  } else
  {
    needed <- is.na(fileSize(lcsv)) | fileSize(lcsv) < 1000 # kB requures evenetual tuning (of check file integrity by opening the file) 
    
    if(format(dates[length(dates)],"%Y")==format(Sys.Date(),'%Y') & file.exists(lcsv[length(lcsv)]))
    {
      thisyear <- read.csv(lcsv[length(lcsv)])
      latestFile <- transDate(end=substr(thisyear$name[length(thisyear$name)],7,16))$endDOY
      latestNeeded <- format(dates[length(dates)],"%Y%j")
      
      if(latestFile<latestNeeded) # eventually we have to consider the time until a file becomes available on the server... 
      {
        needed[length(lcsv)] <- TRUE
      }
    }
  }
  if(length(needed)>0)
  {
    lcsvn <- lcsv[needed]
    rcsvn <- rcsv[needed]
    
    unlink(lcsvn)
    # risky at it might hangs in this loop
    exist <- file.exists(lcsvn)
    while(sum(!exist)!=0)
    {
      
      utils::download.file(rcsvn,lcsvn)
      exist <- file.exists(lcsvn)
      rcsvn <- rcsvn[!exist]
      lcsvn <- lcsvn[!exist]
      Sys.sleep(opts$wait)
    } 
  }
  
  if(!isTRUE(file.exists(lcsv)))
  {
    warning('Not all M*D03 summary files were downloaded.\nConsider to re-run listGeometa()!')
  }
  
  readin <- lcsv[file.exists(lcsv)]
  csvx <- vector(mode = 'list', length = length(readin))
  for(i in seq_along(readin))
  { 
    csvx[[i]] <- as.character(read.csv(readin[i])$name)
  }
  csvx <- unlist(csvx)
  
  filedates <- as.Date(substr(csvx,7,16))
  return(csvx[filedates >= min(dates) & filedates <= max(dates)])    
}

getGeometa <- function(platform=c('terra', 'aqua'),
                       begin = NULL, end = Sys.Date(), 
                       collection=6, forceRefresh=FALSE,...)
{
  
  opts <- MODIS:::combineOptions()
  url <- 'https://ladsweb.modaps.eosdis.nasa.gov/archive/geoMeta/'
  
  collection <- as.integer(collection)
  stopifnot(collection %in% 5:6)
  
  platform <- toupper(platform)
  stopifnot(length(platform) == 1)
  stopifnot(tolower(platform) %in% c('terra', 'aqua'))
  
  dts <- transDate(begin, end)
  
  ## date range
  if(platform=='TERRA')
  {
    begin <- max("2000055",dts$beginDOY) 
  } else
  {
    begin <- max("2002184",dts$beginDOY)
  }
  dts <- transDate(begin, end)
 
  csv <- listGeometa(platform=platform, begin = dts$begin, end = dts$end, collection = collection)
  
  dates <- as.Date(substr(csv,7,16))
  years <- unique(format(dates,'%Y'))  

  basedir <- paste0(opts$auxPath,"geoMeta/",collection,"/",platform,"/")

  # create local directories, if download.files() creates them, we can remove this.
  dirs <- paste0(basedir,unique(format(dates,'%Y')))
  for(u in seq_along(dirs))
  {
    dir.create(dirs[u],recursive=TRUE,showWarnings = FALSE)
  }

  lf <- paste0(basedir,format(dates,'%Y'),"/",csv)
  rf <- paste0("https://ladsweb.modaps.eosdis.nasa.gov/archive/geoMeta/", 
               collection, "/", platform,"/",format(dates,'%Y'),"/",csv)
  
  if(forceRefresh)
  {
    needed <- rep(TRUE, length(lf))
  } else
  {
    fileSize <- fileSize(lf)
    needed   <- is.na(fileSize) | fileSize < 900 # kB requires eventual tuning (of check file integrity by opening the file) 
  }
  if(sum(needed)>0)
  {
    lfn <- lf[needed] 
    rfn <- rf[needed]
    
    # I don't know where the limit is but I got a error: 'Too many open files'
    maxfiles <- 50
    split <- rep(1:100000,each=maxfiles)[1:length(lfn)]
    for(u in 1:max(split))
    {
      try(utils::download.file(rfn[split==u], lfn[split==u], quiet = opts$quiet, mode = "wb"))
      Sys.sleep(opts$wait)
    }
  }
  return(lf[file.exists(lf)])
}

makeFootprintSwath <- function(extent, platform=c('terra', 'aqua'),
                               DayNightFlag = c("D", "N", "B", "X"),
                               begin = NULL, end = Sys.Date(), 
                               collection=6,removeDatecrossers = TRUE,
                               ...) 
{
  
  opts <- MODIS:::combineOptions()

  collection <- as.integer(collection)
  stopifnot(collection %in% 5:6)
  
  platform <- toupper(platform)
  stopifnot(length(platform) == 1)
  stopifnot(tolower(platform) %in% c('terra', 'aqua'))
  
  DayNightFlag <- toupper(DayNightFlag)
  stopifnot(DayNightFlag %in% c("D", "N", "B", "X"))
  
  dts <- transDate(begin, end)
  
  ## date range
  if(platform=='TERRA')
  {
    begin <- max("2000055",dts$beginDOY) 
  } else
  {
    begin <- max("2002184",dts$beginDOY)
  }
  dts <- transDate(begin, end)
  
  geometa <- getGeometa(platform=platform, begin = dts$begin, end = dts$end, collection = collection)
  
  extent <- st_transform(extent,crs = 4326)
  selected <- list()
  rm(footprints)
  for(i in seq_along(geometa))
  { # i=1
    fp <- read.csv(geometa[i])
    
    colnames(fp) <- c("GranuleID", "StartDateTime", "ArchiveSet", "OrbitNumber", "DayNightFlag", 
                      "EastBoundingCoord", "NorthBoundingCoord", "SouthBoundingCoord", "WestBoundingCoord", 
                      "GRingLongitude1", "GRingLongitude2", "GRingLongitude3", "GRingLongitude4", 
                      "GRingLatitude1", "GRingLatitude2", "GRingLatitude3", "GRingLatitude4")
    
    fp <- fp[fp[,"DayNightFlag"] %in% toupper(DayNightFlag),]
    
    if (removeDatecrossers)
    {
      ind <- grep(colnames(fp),pattern="^GRingLongitude")
      prepos <- rowSums(fp[,ind]>=0) 
      
      fp <- fp[prepos == 0 | prepos == 4,]
    }
    
    po <- list()
    for (f in 1:nrow(fp))
    {                    
      po[[f]] <- st_polygon(list(matrix(
        c(fp[f,"GRingLongitude1"], fp[f,"GRingLatitude1"],
          fp[f,"GRingLongitude2"], fp[f,"GRingLatitude2"],
          fp[f,"GRingLongitude3"], fp[f,"GRingLatitude3"], 
          fp[f,"GRingLongitude4"], fp[f,"GRingLatitude4"], 
          fp[f,"GRingLongitude1"], fp[f,"GRingLatitude1"]),
        nc=2, byrow = TRUE)))
    }
    foot <- st_sfc(po,crs=4326)
    #foot <- st_wrap_dateline(footprints, options="WRAPDATELINE=YES")
    foot <- st_sf(data.frame(fp[,c("GranuleID", "StartDateTime", "ArchiveSet", "OrbitNumber", "DayNightFlag")], geom=foot))
    
    # aoi 
    id <- unlist(st_intersects(extent,foot))
    if(!exists('footprints'))
    {
      footprints <- as.data.frame(foot[id,])
    } else
    {
      footprints <- rbind(footprints,as.data.frame(foot[id,]))
    }
  }     
  footprints <- st_sf(footprints)
}


