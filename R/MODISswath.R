#' Get MODIS Swath Granules
#' 
#' @description 
#' Get MODIS swath granules for a specific geographic area, time period and 
#' (optionally) time of day.
#' 
#' @param extent \code{\link[sf]{sf}} oject defining the aoi (other will follow).
#' @param platform \code{Character} of length 1, either MODIS "terra" or "aqua"
#' @param begin Start date of time series. Default NULL which means start with 
#' the first existing image, see \code{\link[MODIS]{transDate}} for mor details.
#' @param end End date of time series. Default is \code{\link{Sys.Date}}.
#' See \code{\link[MODIS]{transDate}} for more details. 
#' @param collection integer or character, valid values are collection 5 or 6
#' @param DayNightFlag A \code{character} vector of allowed day/night flags.
#' This can be an arbitrary combination of \code{"D"} (day), \code{"N"} (night), 
#' \code{"B"} (both), \code{"X"} (not designated). By default, all flags are 
#' accepted.
#' @param removeDatecrossers Default TRUE, remove granules that cross the 
#' dateline as current solution warps polygons accross the globe (any solutions
#' on how to solve this issue are welcome!)
#' @param ... currently only works for the MODIS outDirPath root directory.
#'   
#' @details The retriveval of Spath data follows a three step approach: 
#' 1) download and store CSV summary files. These yearly summary files contain 
#' the filenames of each geometa: MYD03 or MOD03file.
#' 2) base in hte CSV summary file download and store selected geometa files
#' 3) Extract Swath footprint geometries from Geometafiles and intersect it 
#' with the extent.
#' As this function uses caching once files are downloaded if becomes prety fast.
#' 
#' @return 
#' Identified granules as \code{character}.
#' 
#' @author Florain Detsch and Matteo Mattiuzzi
#' 
#' 
#' @examples 
#' \dontrun{
#' 
#' data(meuse)
#' pts = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
#'
#' begin <- '2017001'
#' end   <- '2017010'
#'  
#' fp <- makeFootprintSwath(extent=pts, platform='terra', 
#'             begin = begin, end = end, DayNightFlag = "D")
#' 
#' plot(fp$geometry)
#' fp
#' 
#' }
#' 
#' @export makeFootprintSwath
#' @name makeFootprintSwath
#'
listGeometa = function(platform=c('terra', 'aqua'),
                       begin = NULL, end = Sys.Date(), 
                       collection=6, forceRefresh=FALSE, ...) # 
{
  
  opts <- combineOptions()
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
  
  # create urls
  rcsv <- paste0(url,collection,"/",platform,"/",unique(format(dates,'%Y')),".csv") 
  lcsv <- paste0(csvDir,years,".csv")
  
  if(forceRefresh)
  {
    needed <- rep(TRUE, length(lcsv))  
  } else
  {
    needed <- is.na(fileSize(lcsv)) | fileSize(lcsv) < 500 # kB requires eventual tuning (of check file integrity by opening the file) 
    
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
    warning('Not all M*D03 summary files were downloaded.\nConsider to re-run MODIS:::listGeometa()!')
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
  
  opts <- combineOptions()
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
  
  opts <- combineOptions()
  
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
  
  extent <- sf::st_transform(extent,crs = 4326)
  selected <- list()
  if(exists('footprints'))
  {
    rm(footprints)
  }
  
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
      po[[f]] <- sf::st_polygon(list(matrix(
        c(fp[f,"GRingLongitude1"], fp[f,"GRingLatitude1"],
          fp[f,"GRingLongitude2"], fp[f,"GRingLatitude2"],
          fp[f,"GRingLongitude3"], fp[f,"GRingLatitude3"], 
          fp[f,"GRingLongitude4"], fp[f,"GRingLatitude4"], 
          fp[f,"GRingLongitude1"], fp[f,"GRingLatitude1"]),
        ncol=2, byrow = TRUE)))
    }
    foot <- sf::st_sfc(po,crs=4326)
    #foot <- st_wrap_dateline(footprints, options="WRAPDATELINE=YES")
    foot <- sf::st_sf(data.frame(fp[,c("GranuleID", "StartDateTime", "ArchiveSet", "OrbitNumber", "DayNightFlag")], geom=foot))
    
    # aoi 
    
    id <- unlist(sf::st_intersects(extent,foot))
    
    if(!exists('footprints'))
    {
      footprints <- as.data.frame(foot[id,])
    } else
    {
      footprints <- rbind(footprints,as.data.frame(foot[id,]))
    }
  }     
  footprints <- sf::st_sf(footprints)
}


