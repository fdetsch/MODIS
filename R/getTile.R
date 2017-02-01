#' Get MODIS Tile ID(s)
#' 
#' @description 
#' Get MODIS, MERIS, or SRTM tile ID(s) for a specific geographic area.
#' 
#' @param extent Extent information, see Details. Ignored when \code{tileH} and
#' \code{tileV} are specified.
#' @param tileH,tileV \code{numeric} or \code{character}. Horizontal and 
#' vertical tile number(s) (e.g., \code{tileH = 1:5}), see 
#' \url{https://nsidc.org/data/docs/daac/mod10_modis_snow/landgrid.html}.
#' @param buffer \code{numeric}. Buffers the specified 'extent', negative values
#' are allowed. 
#' \tabular{l}{If 'extent' is a vector type object (e.g. \code{\link{map}}, shp file...)
#' \code{buffer} is specified in map units, usually meters or degrees and  
#' only one value is allowed (e.g., \code{buffer = 0.5}) and \code{\link{gBuffer}} 
#' is used.}
#' \tabular{l}{If 'extent' is a \code{raster}, \code{\link{extend}} or 
#' \code{\link{crop}} are used. In such case the \code{buffer} is meant as number 
#' of pixels to be added/removed.  See \code{\link{extend}} for details.} 
#' \tabular{l}{If 'extent' is an \code{\link{Extent}} object, \code{buffer} is
#' passes to \code{\link{extend}}, see there the details.}
#' @param system \code{character}, defaults to \code{"MODIS"}. Available 
#' alternatives are \code{"MERIS"} and \code{"SRTM"} (see Note).
#' @param zoom \code{logical}, defaults to \code{TRUE}. The interactive mode is 
#' only activated if no other spatial extent (i.e., 'extent', 'tileH', 'tileV') 
#' is specified. If \code{zoom = TRUE}, the first two clicks on the map are 
#' defining the zoom-in area, and the next two clicks are the download area. For 
#' large areas you can set \code{zoom = FALSE}.
#' 
#' @return 
#' A \code{MODISextent} object.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @seealso 
#' \code{\link{extent}}, \code{\link{map}}, \code{\link{search4map}}.
#' 
#' @note 
#' \strong{MODIS} does no longer support the automated download of MERIS and 
#' SRTM data. At least as far as the latter is concerned, easy data access is 
#' granted through \code{\link{getData}}. Despite this limitation, 
#' \code{\link{getTile}} may still be used to identify MERIS and SRTM tile IDs 
#' for manual download based on a user-defined spatial extent.
#' 
#' @details 
#' \describe{
#' \tabular{ll}{
#'   \code{extent}:\cr
#'   \cr 
#'   
#'   If \code{list}:\cr 
#'   \tab Then LatLon coordinates in the following form:\cr 
#'   \tab \code{list(xmin = numeric, xmax = numeric, ymax = numeric, ymin = numeric)}.\cr
#'   \cr
#'   If \code{character}:\cr
#'   \tab The country name of a \code{map} object (see \code{\link{map}}), you 
#'   can use \code{\link{search4map}} to find a map by regular expression.\cr
#'   \tab Or the file name (plus path) of a raster image or ESRI shapefile (shp).\cr
#'   \cr
#'   Other:\cr
#'   \tab If \code{Raster*}.\cr
#'   \tab Using a \code{Raster*} object as \code{extent}, the function 
#'   automatically determines the extent, resolution, and projection. This will 
#'   be used by \code{\link{runGdal}} creating perfectly matching files. If the 
#'   raster file has not a valid CRS string, LatLon is assumed.\cr
#' \tab Using an \code{extent} object, it must be in LatLon, as the extent 
#' object has no projection information attached.\cr
#' \tab If ESRI shapefile (shp) or \code{map} object, a call to 
#' \code{\link{over}} is performed to determine the MODIS tile containg the 
#' extent. This often considerably reduces the number of required tiles, but can 
#' lead to NO-DATA areas if not all tiles had to be downloaded in the bounding 
#' box of 'extent'! 
#' }
#' }
#' 
#' PS:\cr
#' If an extent is specified through \code{tileV, tileH} arguments, no cropping 
#' is performed and the full un-cutted tile(s) (if more than one then also 
#' mosaicked) is/are processed!  
#' 
#' @examples 
#' \dontrun{
#' # ex 1 ############
#' # drawing the extent. NOTE: It is not possible to draw a date-line crossing area!
#' # draw extent with zoom, for smaller extents
#' getTile()
#' 
#' # ex 2 ############
#' # 'extent' specified with a 'Spatial*' object (taken from ?rgdal::readOGR)
#' dsn <- system.file("vectors/Up.tab", package = "rgdal")[1]
#' Up <- rgdal::readOGR(dsn, "Up")
#' getTile(extent = Up)
#' 
#' # ex 3 ############
#' # with 'tileH' and 'tileV'
#' getTile(tileH = 18:19, tileV = 4)
#' 
#' # ex 4 ############
#' # with 'extent' of class 'list'
#' Austria <- list(ymin = 46.12, ymax = 49.3, xmin = 9.2, xmax = 17.47)
#' getTile(extent = Austria)
#' getTile(extent = Austria, buffer = 10)
#' getTile(extent = Austria, buffer = c(0, 10))  # x, y
#' 
#' # ex 5 ############
#' # with 'extent' or 'Raster*' object from "raster" package
#' rasterObject <- raster(xmn = 9.2, xmx = 17.47, ymn = 46.12, ymx = 49.3, 
#'                        crs = "+init=epsg:4326")
#' getTile(extent = rasterObject)
#' getTile(extent = extent(rasterObject))
#' 
#' # also works for projected data
#' rasterObject2 <- projectExtent(rasterObject, crs = "+init=epsg:32633")
#' getTile(extent = rasterObject2)
#' 
#' # ex 6 #################
#' # Character name of a map contained in map("worldHires", plot = FALSE)$names
#' getTile(extent = "Austria")
#' getTile(extent = c("Austria", "Germany"))
#' 
#' # Search for specific map name patterns (use with caution):
#' m1 <- search4map("Per")
#' getTile(extent = m1)
#' 
#' # Or use 'map' objects directly (remember to use map(..., fill = TRUE)): 
#' m2 <- map("state", region = c("new york", "new jersey", "penn"), fill = TRUE)
#' getTile(extent = m2)
#' 
#' # SRTM and MERIS data (does not work at the moment)
#' #getTile(extent = c("austria", "germany", "switzerland"), system = "SRTM")
#' #getTile(extent = c("austria", "germany", "switzerland"), system = "MERIS")
#' }
#' 
#' @export getTile
#' @name getTile
getTile <- function(extent = NULL, tileH = NULL, tileV = NULL, buffer = NULL, system = "MODIS", zoom = TRUE)
{
  if(toupper(system)=='MODIS')
  {
    out <- getTileMODIS(extent = extent, tileH = tileH, tileV = tileV, buffer = buffer, zoom = zoom)
  }
  return(out)
}

getTileMODIS <- function(extent = NULL, tileH = NULL, tileV = NULL, buffer = NULL, zoom = TRUE)
{
  # debug:
  # extent = "austria"; tileH = NULL; tileV = NULL; buffer = NULL; zoom=TRUE
  
  # if extent is a former result of getTile
  if (inherits(extent,"MODISextent"))
  {
    return(extent)
  }
  
  # if 'tileH' and 'tileV' are present, exit function after that
  if (!is.null(tileH) & !is.null(tileV)) 
  {
    tiltab <- tiletable
    
    tileH <- as.numeric(tileH)
    tileV <- as.numeric(tileV)
    
    tt     <- tiltab[(tiltab$ih %in% tileH) & (tiltab$iv %in% tileV) & (tiltab$xmin>-999),]
    ext    <- extent(c(min(tt$xmin),max(tt$xmax),min(tt$ymin),max(tt$ymax)))
    
    tt$iv <- sprintf("%02d", tt$iv)
    tt$ih <- sprintf("%02d", tt$ih)
    
    tileH <- sprintf("%02d",tileH)
    tileV <- sprintf("%02d",tileV)
    
    tilesSUB <- as.character(apply(tt,1, function(x) {
      paste0("h", x[2], "v",  x[1])
    }))
    tiles <- as.character(sapply(tileH, function(x){paste0("h", x, "v", tileV)}))
    
    if (!all(tiles %in% tilesSUB))  # all possible tiles vs all available
    {
      rem <- paste0(tiles[!tiles %in% tilesSUB],collapse=", ")
      cat(paste0("The following 'tiles' do not exist:\n",rem,"\n"))
      tiles <- tilesSUB
      tileH <- unique(tt$ih)
      tileV <- unique(tt$iv)
    }
    result <- list( tile = tiles, tileH = as.numeric(tileH), tileV = as.numeric(tileV), extent = extent, system = 'MODIS', target=NULL)
    class(result) <- "MODISextent"
    return(result)
  }

  usePoly <- TRUE # "usePoly=F" works without suggested packages, "usePoly=T" only for MODIS system + having rgdal and rgeos installed
  target  <- NULL  # if extent is a raster*/Spatial* and has a different proj it is changed, the information is added here
  fromMap <- FALSE
  crs <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' # shall be projection(MODIS:::sr)
  
  # if extent is null, do mapSelect. Output class extent. 
  if(is.null(extent))
  {
    extent  <- mapSelect(zoom=zoom)
    # outProj <- crs # not sure about that
  }  
  
  # list input. Output class extent. TODO a check for correctness
  if (inherits(extent, "list")) 
  {
    if (length(extent$extent) == 4) 
    {
      extent <- extent$extent
    }
    extent <- extent(c(min(extent$xmin,extent$xmax), max(extent$xmin,extent$xmax), min(extent$ymin,extent$ymax), max(extent$ymin,extent$ymax)))
  }
  
  # filename string to Raster/vector conversion
  if(inherits(extent,"character") & length(extent)==1) # lengh>1 it should be only a mapname for maps:::map
  {
    if (raster::extension(extent)=='.shp')
    {
      extent <- shapefile(extent)
    } else 
    {
      if (file.exists(extent))
      {
        extent <- raster(extent)
      }
    }
  }  
  
  # character (country name of MAP) to maps::map conversion     
  if (inherits(extent, "character"))
  {
    try(testm <- maps::map("worldHires", extent, plot = FALSE),silent = TRUE)
    if (!exists("testm"))
    {
      stop(paste0("Country name not valid. Check availability/spelling, i.e. try if it works with: map('worldHires,'",extent, "'), or use '?search4map' function"))
    }
    extent <- maps::map("worldHires", extent, plot = FALSE, fill=TRUE)
  }
  
  # maps::map (from mapdata/maps) to SpatialPolygons
  if (inherits(extent, "map"))
  {
    extent  <- m2SP(extent, extent$names, CRS(crs))
  }
  
  if(!is.null(buffer))
  {
    if(length(grep(class(extent), pattern="^Spatial*")==1))
    {
      if (length(buffer)>1)
      {
        buffer <- buffer[1]
        warning(paste0("'buffer' on a vector object must have length==1. Using only the first element of 'buffer': ",buffer))
      }
      if(isLonLat(extent))
      {
        # gBuffer not allowed on LatLon, use fake CRS to bypass this. Found in: 
        # http://stackoverflow.com/questions/9735466/how-to-compute-a-line-buffer-with-spatiallinesdataframe
        win                     <- getOption("warn") 
        options(warn=-2)
        inproj                  <- sp::proj4string(extent)
        sp::proj4string(extent) <- CRS("+init=epsg:3395")
        extent                  <- gBuffer(extent,width=buffer)
        sp::proj4string(extent) <- CRS(inproj)
        options(warn=win)
      } else
      {
        extent <- gBuffer(extent, width=buffer)
      }
    } else if (length(grep(class(extent), pattern="^Raster*")==1))
    {
      if (buffer[1]>0) # this is a simplification. As in theory we could have buffer=c(-1,1)? 
      {
        extent <- extend(extent,buffer)  
      } else
      {
        extent <- crop(extent,buffer)  
      }  
    } else if(inherits(extent,"Extent"))
    {
      extent <- extend(extent,buffer)  
    }
  }
  
  # this needs to be done in order to use rgdal:::over to intersect geometies 
  if (length(grep(class(extent),pattern="Raster*"))==1 | length(grep(class(extent),pattern="Spatial*"))==1)
  {
    target <- list(outProj = projection(extent), extent = extent(extent), pixelSize = NULL) 
    
    if(length(grep(class(extent), pattern="Raster*"))==1)
    {
      target$pixelSize <- res(extent)
      extent <- extent(projectExtent(extent,crs))
    } else
    {
      extent <- spTransform(extent,CRS(crs))
    }
  } 
  if(inherits(extent,'Extent'))
  {
    extent <- as(extent, 'SpatialPolygons')
  }
  
  if (is.na(sp::proj4string(extent)))
  {
    sp::proj4string(extent) <- crs
  }
  
  selected <- sr[extent,] # sr 
  
  tileH  <- unique(as.numeric(selected@data$h))
  tileV  <- unique(as.numeric(selected@data$v))
  
  result <- as.character(apply(selected@data,1,function(x) {paste("h",sprintf("%02d",x[2]),"v",sprintf("%02d",x[3]),sep="")}))
  result <- list(tile = result, tileH = tileH, tileV = tileV, extent = extent, system = 'MODIS', target = target)
  class(result) <- "MODISextent"
  return(result)
}
 



mapSelect <- function(zoom=TRUE)
{
  dev.new(width=9,height=7)
  maps::map("worldHires")
  map.axes() 
  grid(36,18,col="blue",lwd=0.5)
  abline(h=0,col="yellow",lwd=1)
  if(zoom) 
  {
    title("ZOOM-IN by selecting UL and LR points with the mouse!")            
    # code taken from function raster::drawExtent
    loc1 <- locator(n = 1, type = "p", pch = "+", col = "red")
    loc2 <- locator(n = 1, type = "p", pch = "+", col = "red")
    loc  <- rbind(unlist(loc1), unlist(loc2))
    #
    p    <- rbind(c(min(loc[, "x"]), min(loc[,"y"])), c(min(loc[, "x"]), max(loc[, "y"])),
                  c(max(loc[, "x"]), max(loc[, "y"])), c(max(loc[, "x"]), min(loc[,"y"])), c(min(loc[, "x"]),min(loc[,"y"])))
    lines(p, col = "red")            
    
    Sys.sleep(0.5)
    maps::map("worldHires",xlim=c(min(loc[, "x"]),max(loc[, "x"])),ylim=c(min(loc[,"y"]),max(loc[, "y"])))
    map.axes() 
    grid(36,18,col="blue",lwd=0.5)            
  }
  title("Set UL and LR points with the mouse!")
  # code taken from function raster::drawExtent
  loc1 <- locator(n = 1, type = "p", pch = "+", col = "red")
  loc2 <- locator(n = 1, type = "p", pch = "+", col = "red")
  loc  <- rbind(unlist(loc1), unlist(loc2))
  #
  p    <- rbind(c(min(loc[, "x"]), min(loc[,"y"])), c(min(loc[, "x"]), max(loc[, "y"])),
                c(max(loc[, "x"]), max(loc[, "y"])), c(max(loc[, "x"]), min(loc[,"y"])), c(min(loc[, "x"]),min(loc[,"y"])))
  lines(p, col = "red")
  
  extent <- extent(c(min(loc[, "x"]), max(loc[, "x"]), min(loc[, "y"]), max(loc[, "y"])))
  
  Sys.sleep(0.6)
  if(zoom)
  {
    text(x=min(loc[, "x"])+((max(loc[, "x"])-min(loc[, "x"]))/2),y=min(loc[,"y"])+((max(loc[, "y"])-min(loc[,"y"]))/2),"OK, extent is set!\nclosing window...",cex=3)    
  } else 
  {
    text(x=-4,y=21,"OK, extent is set!\nclosing window...",cex=5)
  }
  Sys.sleep(1.6)
  dev.off()
  return(extent)
}

# in order to avoid dependency the following functions are copied (nearly identical) from 'maptools' (v 0.8-26) package
# function 'map2SpatialPolygons' from package 'maptools' 
m2SP <- function (map, IDs, proj4string = CRS(as.character(NA))) 
{
  #require(maps)
  if (missing(IDs)) 
    stop("IDs required")
  xyList <- st1(cbind(map$x, map$y))
  if (length(xyList) != length(IDs)) 
    stop("map and IDs differ in length")
  tab <- table(factor(IDs))
  n <- length(tab)
  IDss <- names(tab)
  reg <- match(IDs, IDss)
  belongs <- lapply(1:n, function(x) which(x == reg))
  Srl <- vector(mode = "list", length = n)
  for (i in 1:n) {
    nParts <- length(belongs[[i]])
    srl <- vector(mode = "list", length = nParts)
    for (j in 1:nParts) {
      crds <- xyList[[belongs[[i]][j]]]
      if (nrow(crds) == 3) 
        crds <- rbind(crds, crds[1, ])
      srl[[j]] <- Polygon(coords = crds)
    }
    Srl[[i]] <- Polygons(srl, ID = IDss[i])
  }
  res <- as.SpatialPolygons.PolygonsList(Srl, proj4string = proj4string)
  res
}
# function 'map2SpatialLines' from package 'maptools' 
m2SL <- function (map, IDs = NULL, proj4string = CRS(as.character(NA))) 
{
  #    require(maps)
  xyList <- st1(cbind(map$x, map$y))
  if (is.null(IDs)) 
    IDs <- as.character(1:length(xyList))
  if (length(xyList) != length(IDs)) 
    stop("map and IDs differ in length")
  tab <- table(factor(IDs))
  n <- length(tab)
  IDss <- names(tab)
  reg <- match(IDs, IDss)
  belongs <- lapply(1:n, function(x) which(x == reg))
  Srl <- vector(mode = "list", length = n)
  for (i in 1:n) {
    nParts <- length(belongs[[i]])
    srl <- vector(mode = "list", length = nParts)
    for (j in 1:nParts) {
      crds <- xyList[[belongs[[i]][j]]]
      if (nrow(crds) > 1) 
        srl[[j]] <- Line(coords = crds)
      else srl[[j]] <- Line(coords = rbind(crds, crds))
    }
    Srl[[i]] <- Lines(srl, ID = IDss[i])
  }
  res <- SpatialLines(Srl, proj4string = proj4string)
  res
}


# function '.NAmat2xyList' from package 'maptools' 
st1 <- function (xy) 
{
  NAs <- unclass(attr(na.omit(xy), "na.action"))
  if ((length(NAs) == 1L) && (NAs == nrow(xy))) {
    xy <- xy[-nrow(xy)]
    NAs <- NULL
  }
  diffNAs <- diff(NAs)
  if (any(diffNAs == 1)) {
    xy <- xy[-(NAs[which(diffNAs == 1)] + 1), ]
    NAs <- unclass(attr(na.omit(xy), "na.action"))
  }
  nParts <- length(NAs) + 1L
  if (!is.null(NAs) && nrow(xy) == NAs[length(NAs)]) 
    nParts <- nParts - 1
  res <- vector(mode = "list", length = nParts)
  from <- integer(nParts)
  to <- integer(nParts)
  from[1] <- 1
  to[nParts] <- nrow(xy)
  if (!is.null(NAs) && nrow(xy) == NAs[length(NAs)]) 
    to[nParts] <- to[nParts] - 1
  if (nParts > 1) {
    for (i in 2:nParts) {
      to[(i - 1)] <- NAs[(i - 1)] - 1
      from[i] <- NAs[(i - 1)] + 1
    }
  }
  for (i in 1:nParts) res[[i]] <- xy[from[i]:to[i], , drop = FALSE]
  res
}
