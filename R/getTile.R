#' Get MODIS Tile ID(s)
#' 
#' @description 
#' Get MODIS tile ID(s) for a specific geographic area.
#' 
#' @param x Extent information, see Details. Ignored if \code{tileH} and
#' \code{tileV} are specified.
#' @param tileH,tileV \code{numeric} or \code{character}. Horizontal and 
#' vertical tile number(s) of the 
#' \href{https://nsidc.org/data/docs/daac/mod10_modis_snow/landgrid.html}{MODIS Sinusoidal grid}
#' (e.g., \code{tileH = 1:5}). If specified, no cropping is performed and the 
#' full tile(s) (if more than one then also mosaicked) is (are) processed!  
#' 
#' @return 
#' A \code{MODISextent} object.
#' 
#' @author 
#' Matteo Mattiuzzi, Florian Detsch
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
#' If \code{x} is of class (see Examples for use cases)
#' \tabular{ll}{
#'   \code{missing}:\cr
#'   \tab If 'tileH,tileV' are specified, 'x' will be ignored. If no such tile 
#'   indices are provided and 'x' is missing, a viewer window pops up that 
#'   allows interactive tile selection from the global MODIS Sinusoidal grid.\cr
#'   \cr 
#'   \code{character}:\cr
#'   \tab Either the country name of a \code{map} object (see \code{\link{map}})
#'   or a valid file path of a raster image or ESRI shapefile (shp). The former 
#'   approach also supports pattern matching via regular expressions.\cr
#'   \cr
#'   \code{Raster*}:\cr
#'   \tab Spatial extent, resolution, and projection of the specified 
#'   \code{Raster*} are determined automatically. This information is used by 
#'   \code{\link{runGdal}} to create perfectly matching files. If the 
#'   \code{Raster*} comes with no valid CRS, 
#'   \href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326} is assumed.\cr
#'   \cr
#'   \code{Extent}:\cr
#'   \tab Boundary coordinates from \code{Extent} objects are assumed to be in 
#'   \href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326} as well as 
#'   such objects have no projection information attached.\cr
#'   \cr
#'   Other:\cr
#'   \tab \code{Spatial}, \code{sf}, or \code{map} object.
#' }
#' 
#' @examples 
#' \dontrun{
#' # ex 1 ############
#' # interactive tile selection
#' getTile()
#' 
#' # ex 2: Spatial (taken from ?rgdal::readOGR) ############
#' dsn <- system.file("vectors/Up.tab", package = "rgdal")[1]
#' Up <- rgdal::readOGR(dsn, "Up")
#' getTile(Up)
#' 
#' # ex 3: sf ############
#' library(mapview)
#' getTile(franconia)
#' 
#' # ex 4: tileH,tileV ############
#' getTile(tileH = 18:19, tileV = 4)
#' 
#' # ex 5: Raster* with valid CRS ############
#' rst1 <- raster(xmn = 9.2, xmx = 17.47, ymn = 46.12, ymx = 49.3)
#' getTile(rst1)
#' 
#' # this also works for projected data
#' rst3 <- projectExtent(rst1, crs = "+init=epsg:32633")
#' getTile(rst3)
#' 
#' # ex 6: Raster* without CRS or, alternatively, Extent -> treated as EPSG:4326 ############
#' mat2 <- matrix(seq(180 * 360), byrow = TRUE, ncol = 360)
#' rst2 <- raster(mat2)
#' getTile(rst2)
#' getTile(extent(rst1))
#' 
#' # ex 7: map names as returned by search4map() ############
#' getTile("Austria")
#' getTile(c("Austria", "Germany"))
#' 
#' # or search for specific map name patterns (use with caution):
#' m1 <- search4map("Per")
#' getTile(m1)
#' 
#' # or use 'map' objects directly (remember to use map(..., fill = TRUE)): 
#' m2 <- map("state", region = "new jersey", fill = TRUE)
#' getTile(m2)
#' }
#' 
#' @export getTile
#' @name getTile
getTile <- function(x = NULL, tileH = NULL, tileV = NULL) {

  if (all(!is.null(tileH), !is.null(tileV))) {
    if (!is.numeric(tileH)) tileH <- as.numeric(tileH)
    if (!is.numeric(tileV)) tileV <- as.numeric(tileV)
    
    tt <- tiletable[(tiletable$ih %in% tileH) & 
                      (tiletable$iv %in% tileV) & (tiletable$xmin >- 999), ]
    ext <- raster::extent(c(min(tt$xmin), max(tt$xmax), min(tt$ymin), max(tt$ymax)))
    
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
    result <- list( tile = tiles, tileH = as.numeric(tileH), tileV = as.numeric(tileV), extent = ext, system = 'MODIS', target=NULL)
    class(result) <- "MODISextent"
    return(result)
  }

  target  <- NULL  # if extent is a raster*/Spatial* and has a different proj it is changed, the information is added here
  fromMap <- FALSE
  prj <- sp::CRS("+init=epsg:4326")
  
  # if 'x' is null, do mapSelect. Output class extent. 
  if (is.null(x)) {
    x <- mapSelect()
  }  
  
  # filename string to Raster/vector conversion
  if(inherits(x,"character") & length(x)==1) # lengh>1 it should be only a mapname for maps:::map
  {
    if (raster::extension(x)=='.shp')
    {
      x <- shapefile(x)
    } else 
    {
      if (file.exists(x))
      {
        x <- raster(x)
      }
    }
  }  
  
  # character (country name of MAP) to maps::map conversion     
  if (inherits(x, "character"))
  {
    try(testm <- maps::map("worldHires", x, plot = FALSE),silent = TRUE)
    if (!exists("testm"))
    {
      stop(paste0("Country name not valid. Check availability/spelling, i.e. try if it works with: map('worldHires,'",x, "'), or use '?search4map' function"))
    }
    x <- maps::map("worldHires", x, plot = FALSE, fill=TRUE)
  }
  
  # maps::map (from mapdata/maps) to SpatialPolygons
  if (inherits(x, "map")) {
    x  <- m2SP(x, x$names, prj)
  }
  
  # this needs to be done in order to use rgdal:::over to intersect geometies 
  if (inherits(x, c("Raster", "Spatial"))) {
    target <- list(outProj = raster::projection(x)
                   , extent = raster::extent(x)
                   , pixelSize = NULL) 
    
    if (inherits(x, "Raster"))
      target$pixelSize <- raster::res(x)
      
    # if required, spTransform() extent object
    if (!raster::compareCRS(x, prj) & !is.na(target$outProj)) {
      x <- if (inherits(x, "Raster")) {
        raster::extent(raster::projectExtent(x, prj))
      } else {
        sp::spTransform(x, prj)
      }
    } else if (is.na(target$outProj)) {
      raster::projection(x) <- prj
    }
    
  # sf method  
  } else if (inherits(x, "sf")) {
    target <- list(outProj = sf::st_crs(x)$proj4string
                   , extent = raster::extent(sf::st_bbox(x)[c(1, 3, 2, 4)])
                   , pixelSize = NULL)
    
    if (!raster::compareCRS(target$outProj, prj)) {
      x <- sf::st_transform(x, prj@projargs)
    }
  }
  
  if (inherits(x, 'Extent')) {
    x <- as(x, 'SpatialPolygons')
    sp::proj4string(x) <- prj
  }
  
  if (inherits(x, "sf"))
    x <- methods::as(x, "Spatial")

  selected <- raster::crop(sr, x)
  tileH  <- unique(as.numeric(selected@data$h))
  tileV  <- unique(as.numeric(selected@data$v))
  
  result <- as.character(apply(selected@data,1,function(x) {paste("h",sprintf("%02d",x[2]),"v",sprintf("%02d",x[3]),sep="")}))
  result <- list(tile = result, tileH = tileH, tileV = tileV, extent = x, system = 'MODIS', target = target)
  class(result) <- "MODISextent"
  return(result)
}
 


mapSelect <- function() {
  
  grd <- sf::st_as_sf(sr, quiet = TRUE)
  sel <- mapedit::selectFeatures(grd)

  return(sel)
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
