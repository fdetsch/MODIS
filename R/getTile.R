#' Get MODIS Tile ID(s)
#' 
#' @description 
#' Get MODIS tile ID(s) for a specific geographic area.
#' 
#' @param extent Extent information, see Details. Ignored when \code{tileH} and
#' \code{tileV} are specified.
#' @param tileH,tileV \code{numeric} or \code{character}. Horizontal and 
#' vertical tile number(s) (e.g., \code{tileH = 1:5}), see 
#' \url{https://nsidc.org/data/docs/daac/mod10_modis_snow/landgrid.html}.
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
#' \strong{MODIS} does no longer support the tile identification and automated 
#' download of MERIS and SRTM data. At least as far as the latter is concerned, 
#' easy data access is granted through \code{\link{getData}}. 
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
#' }
#' 
#' @export getTile
#' @name getTile
getTile <- function(extent = NULL, tileH = NULL, tileV = NULL) {

  # debug:
  # extent = "austria"; tileH = NULL; tileV = NULL
  
  # if extent is a former result of getTile
  if (inherits(extent, "MODISextent"))
    return(extent)

  # if 'tileH' and 'tileV' are present, exit function after that
  if (!is.null(tileH) & !is.null(tileV)) {

    tileH <- as.numeric(tileH)
    tileV <- as.numeric(tileV)
    
    tt <- tiletable[(tiletable$ih %in% tileH) & 
                      (tiletable$iv %in% tileV) & (tiletable$xmin >- 999), ]
    ext <- extent(c(min(tt$xmin), max(tt$xmax), min(tt$ymin), max(tt$ymax)))
    
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

  target  <- NULL  # if extent is a raster*/Spatial* and has a different proj it is changed, the information is added here
  fromMap <- FALSE
  prj <- sp::CRS("+init=epsg:4326")
  
  # if extent is null, do mapSelect. Output class extent. 
  if(is.null(extent)) {
    # extent  <- mapSelect()
    grd <- sf::st_as_sf(sr, quiet = TRUE)
    sel <- mapedit::selectFeatures(grd, style_true = list(fillColor = "red"), 
                                   style_false = list(fillColor = "blue")) 
    extent <- as(sel, "Spatial")
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
    extent  <- m2SP(extent, extent$names, prj)
  }
  
  # this needs to be done in order to use rgdal:::over to intersect geometies 
  if (inherits(extent, c("Raster", "Spatial"))) {
    target <- list(outProj = raster::projection(extent)
                   , extent = raster::extent(extent)
                   , pixelSize = NULL) 

    if (inherits(extent, "Raster"))
      target$pixelSize <- raster::res(extent)
      
    # if required, spTransform() extent object
    if (!raster::compareCRS(extent, prj)) {
      extent <- if (inherits(extent, "Raster")) {
        raster::extent(raster::projectExtent(extent, prj))
      } else {
        sp::spTransform(extent, prj)
      }
    }
  } 
  
  if(inherits(extent,'Extent'))
  {
    extent <- as(extent, 'SpatialPolygons')
  }
  
  if (is.na(sp::proj4string(extent)))
  {
    sp::proj4string(extent) <- prj
  }
  
  selected <- raster::crop(sr, extent)
  
  tileH  <- unique(as.numeric(selected@data$h))
  tileV  <- unique(as.numeric(selected@data$v))
  
  result <- as.character(apply(selected@data,1,function(x) {paste("h",sprintf("%02d",x[2]),"v",sprintf("%02d",x[3]),sep="")}))
  result <- list(tile = result, tileH = tileH, tileV = tileV, extent = extent, system = 'MODIS', target = target)
  class(result) <- "MODISextent"
  return(result)
}
 



mapSelect <- function() {
  
  shp <- system.file("external", "modis_latlonWGS84_grid_world.shp", package = "MODIS")
  grd <- sf::st_read(shp, quiet = TRUE)

  sel <- mapedit::selectFeatures(grd, style_true = list(fillColor = "red"), 
                                 style_false = list(fillColor = "blue")) 
  return(as(sel, "Spatial"))
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
