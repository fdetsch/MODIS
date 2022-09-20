#' Minor MODIS Package Functions
#'
#' @description 
#' Compendium of minor **MODIS** package-related functions.
#' 
#' @param pattern Regular expression passed to [grep()].
#' @param database `character`. Defaults to `"worldHires"`, see [maps::map()] 
#'   for available options.
#' @param plot `logical`, defaults to `FALSE`. If `TRUE`, search results are 
#'   displayed.
#' 
#' @return 
#' A `list` of length 2. The first entry is the call to create the given 
#' map, whereas the second entry represents the names of areas within the 
#' search. 
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @seealso 
#' [getTile()], [maps::map()], [grep()].
#' 
#' @examples 
#' \donttest{
#' search4map()
#' 
#' search4map(pattern="USA",plot=TRUE)
#' search4map(database="state",plot=TRUE)
#' 
#' search4map(database="italy",pattern="Bolz",plot=TRUE)
#' 
#' search4map(pattern="Sicily",plot=TRUE)
#' }
#' 
#' @name minorFuns
NULL

##########################################
# central setting for stubbornness 
stubborn <- function(level = "high") {
  
  ## supported 'character' levels
  levels <- c("low", "medium", "high", "veryhigh", "extreme")
  
  ## if stubbornness is a 'character', try to find it in 'levels' or convert it 
  ## to regular 'numeric'
  if (!is.numeric(level) & !tolower(level) %in% levels) {
    level <- suppressWarnings(try(as.numeric(level), silent = TRUE))
    if (inherits(level, "try-error") | is.na(level))
      stop("Unrecognised 'stubbornness' level!")
  }
  
  ## round or convert 'character' level to 'numeric'
  if (is.numeric(level)) {
    round(level)    
  } else { 
    c(5, 15, 50, 100, 1000)[which(tolower(level) == levels)]
  }
}


# ## seems to be deprecated or remnant from an early MODIS version
# checksizefun <- function(file,sizeInfo=NULL,flexB=0)
# {
#     # determine reference size
#     if (is.null(sizeInfo))
#     {
#         xmlfile  <- paste0(file,".xml")
#         xmlfile  <- xmlParse(xmlfile)
#         MetaSize <- getNodeSet(xmlfile, "/GranuleMetaDataFile/GranuleURMetaData/DataFiles/DataFileContainer/FileSize" )
#         MetaSize <- as.numeric(xmlValue(MetaSize[[1]])) # expected filesize
#     } else 
#     {
#         MetaSize <- as.numeric(sizeInfo[which(sizeInfo[,1]==basename(file)),2])
#     }
#     
#     if(length(MetaSize)==0)
#     {
#         res  <- list(MetaSize=NULL,FileSize=NULL,isOK=NULL)
#         return(res)
#     }
#     
#     FileSize <- as.numeric(fileSize(file))
#     if (flexB!=0)
#     {
#         isOK <- (MetaSize >= FileSize-flexB & MetaSize <= FileSize+flexB)
#     } else 
#     {
#         isOK <- (MetaSize == FileSize)
#     }
#     res  <- list(MetaSize=MetaSize,FileSize=FileSize,isOK=as.logical(isOK))
# return(res)
# }


#' @describeIn minorFuns Simplifies search for **mapdata**-based extents
#' @aliases search4map
#' @export search4map
search4map <- function(pattern="",database='worldHires',plot=FALSE)
{
  areas <- grep(x=maps::map(database,plot=FALSE)$names,pattern=pattern,value=TRUE,ignore.case=TRUE)
  
  if (length(areas)==0)
  {
    cat("No country (region or island) found! please change your pattern!\n")
    return(invisible(NULL))
  } else 
  {
  
  if (plot)
  {
    maps::map(database,areas)
    map.axes() 
    box()
    grid(36,18,col="blue",lwd=0.5)
    
    if(length(areas)>4) 
    {
      subareas <- paste(areas[1:3],collapse=", ") 
      title(c(paste(subareas,"and",(length(areas)-3),"other")))
    } else 
    {
      title(areas)
    }
  }
  return(areas=areas)
  }
}


getExtension = function(dataFormat, ...) {
  
  ### . legacy ----
  
  dataFormat = toupper(dataFormat)
  if(dataFormat %in% c("HDF-EOS","HDF4IMAGE")) # MRT + GDAL
  {
    return(".hdf")
  } else if (dataFormat %in% c("GTIFF", "GEOTIFF"))  # MRT + GDAL
  {
    return(".tif")
  } else if (dataFormat =="RAW BINARY")  # MRT + GDAL
  {
    return(".hdr")
  } else if (dataFormat == "ENVI") 
  {
    return("") # should generate a '.hdr' file + a file without extension
  } else if (dataFormat == "FITS") 
  {
    return(NA)
  } else if (dataFormat == "ILWIS")
  {
    return(".mpr")
  } else 
  {
    
    ### . gdalinfo (if available) ----
    
    if (system("gdalinfo --version", ignore.stdout = TRUE) == 0) {
      stdout = system(paste("gdalinfo --format", dataFormat), intern = TRUE)
      
      ext = regmatches(stdout, regexpr("Extension[s]{0,1}: [[:alnum:] ]*", stdout))
      if (length(ext) > 0) {
        ext = gsub("Extension[s]{0,1}: ", "", ext)
        if (ext != "") {
          paste0(".", sapply(strsplit(ext, " "), "[[", 1))
        } else ext
      } else {
        NA
      }
    } else NA
  }
}


isSupported <- function(x) 
{
  fname <- basename(x)
  
  iw   <- options()$warn 
  options(warn=-1)
  on.exit(options(warn=iw))
  
  res <- sapply(fname,function(y) 
  {
    product <- getProduct(y,quiet=TRUE)
    
    if (is.null(product))
    {
      return(FALSE)
    } else 
    {
      secName <- defineName(product@request)
      
      if (product@TYPE[1] == "Tile") 
      {
        Tpat    <- "h[0-3][0-9]v[0-1][0-9]" # to enhance
        return(all((grep(secName["TILE"],pattern=Tpat)) + (substr(secName["DATE"],1,1) == "A") + (length(secName)==6)))
        
      } else if (product@TYPE[1] == "CMG") 
      {
        return(all((substr(secName["DATE"],1,1) == "A") + (length(secName)==5)))
        
      } else if (product@TYPE[1] == "Swath")  # actually no support for Swath data!
      {
        #             return(all((substr(secName["DATE"],1,1) == "A") + (length(secName)==6)))
        #                } else {
        return(FALSE)
      }
    }
  })
return(unlist(res))
}

# TODO enhancement of SENSOR/PRODUCT detection capabilities! 
# the methods below are based on the results of strsplit().

defineName <- function(x) # "x" is a MODIS or filename
{
  
  if(missing(x)) 
  {
    stop("Error in function 'defineName', x is missing, must be a MODIS filename!")
  } else 
  {
    fname   <- basename(x)
    secName <- strsplit(fname,"\\.")[[1]] # for splitting with more signes "[._-]"
    
    sensor="MODIS"
    
    ###################################
    # NAME definitions (is File-specific!)
    #########################
    # MODIS
    if (sensor=="MODIS")
    {
      product <- getProduct(x=secName[1],quiet=TRUE)
      if (product@TYPE=="Tile") 
      {
        names(secName) <- c("PRODUCT","DATE","TILE","CCC","PROCESSINGDATE","FORMAT")
      } else if (product@TYPE=="CMG") 
      {
        names(secName) <- c("PRODUCT","DATE","CCC","PROCESSINGDATE","FORMAT")
      } else if (product@TYPE=="Swath") 
      { 
        names(secName) <- c("PRODUCT","DATE","TIME","CCC","PROCESSINGDATE","FORMAT")
      } else 
      {
        stop("Not a MODIS 'Tile', 'CMG' or 'Swath'!")
      }
    }  # XXX else if .... add Products here
  }
  return(secName)
}

# this function selects elements of a list by "row".
listPather <- function(x,index)
{
    x   <- as.list(x)
    res <- list()
    
    for (i in seq_along(x))
    {
        res[[i]] <- x[[i]][index]
    }
    names(res) <- names(x)
    return(res)
}

# list files in a Url
filesUrl <- function(url)
{

  if (substr(url,nchar(url),nchar(url))!="/")
  {
    url <- paste0(url,"/") 
  }
  
  iw = options()$warn 
  options(warn = -1)
  on.exit(options(warn = iw))
  
  h <- curl::new_handle(
    connecttimeout = 60L
  )
  
  ## laads, nsidc require login
  crd = credentials()
  usr = crd$login; pwd = crd$password
  
  curl::handle_setopt(
    handle = h,
    httpauth = 1,
    userpwd = paste0(usr, ":", pwd)
  )
  
  ## establish connection
  is_laads = grepl("ladsweb", url)
  con = curl::curl(
    ifelse(is_laads, gsub("/$", ".csv", url), url)
    , handle = h
  )
  on.exit(
    try(
      close(con)
      , silent = TRUE)
  )
  
  ## read online content
  if (!is_laads) {
    co = readLines(con)
    close(con)
    
    # extract '<a href=...> nodes
    pttrn = '<a href=\"[[:graph:]]{1,}\">[[:graph:]]{1,}</a>'
    tmp = sapply(co, function(i) {
      regmatches(i, regexpr(pttrn, i))
    })
    
    spl1 = sapply(strsplit(unlist(tmp), ">"), "[[", 2)
    fnames = as.character(sapply(strsplit(spl1, "<"), "[[", 1))
  } else {
    tmp = utils::read.csv(con, colClasses = "character") # closes 'con' automatically
    fnames = tmp$name[grep("[^NOTICE]", tmp$name)]
  }
  
  ## format and return    
  fnames <- gsub(fnames,pattern="/",replacement="")
  return(fnames)
}


#http://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
makeRandomString <- function(n=1, length=12)
{
    randomString <- c(1:n) # initialize vector
    for (i in 1:n)
    {
        randomString[i] <- paste0(sample(c(0:9, letters, LETTERS),
        length, replace=TRUE),collapse="")
    }   
    return(randomString)
}

# this function care about the download of files. Based on remotePath (result of genString) it alterates the effort on available sources and stops after succeded download or by reacing the stubbornness thresshold.
ModisFileDownloader <- function(x, ...)
{
    x <- basename(x)
    
    opts <- combineOptions(...)
    
    opts$stubbornness <- stubborn(opts$stubbornness)
    opts$quiet <- as.logical(opts$quiet)
    
    iw <- options()$warn 
    options(warn=-1)
    on.exit(options(warn=iw))

    out <- rep(NA,length=length(x))
    
    for (a in seq_along(x))
    {  # a=1
        path <- do.call(genString, c(list(x = x[a], collection = getCollection(x[a], quiet = TRUE)), opts))
        path$localPath <- setPath(path$localPath)
        destfile       <- paste0(path$localPath,x[a])
        
        hv <- seq_along(opts$MODISserverOrder)
        hv <- rep(hv,length=opts$stubbornness)
        g=1
        while(g <= opts$stubbornness) 
        {     
          if (!opts$quiet)
          {
            if(length(path$remotePath) > 1 & opts$dlmethod == "aria2")
            {
              cat("\nMultisocket connection to:",paste(names(path$remotePath), collapse = ' and '),"\n############################\n")
            } else
            {
              cat(
                sprintf(
                  "\nGetting file %s from: %s\n############################\n"
                  , x
                  , opts$MODISserverOrder[hv[g]]
                )
              )
            }
          }
          
          # we need to check the behaviour of aria on windows...
          if(opts$dlmethod=="aria2")
          {
            out[a] <- system(paste0("aria2c -x2 --file-allocation=none --allow-overwrite=true ", paste(path$remotePath,x[a],sep="/",collapse=" "), " -d ", dirname(destfile)))
          } else
          {

            ## if server is 'LPDAAC' or 'LAADS', consider MODISserverOrder
            if (any(names(path$remotePath) %in% opts$MODISserverOrder[hv[g]])) {
              id_remotepath <- which(names(path$remotePath) == opts$MODISserverOrder[hv[g]])
            
            ## if not (e.g. when server is 'NTSG'), simply take the first `path$remotePath` entry
            } else {
              id_remotepath <- 1
            }
              
            server <- names(path$remotePath)
            if (length(server) > 1) {
              server <- server[which(server %in% opts$MODISserverOrder[hv[g]])]
            }
              
            infile <- paste(path$remotePath[id_remotepath], x[a], sep = "/", 
                            collapse = "")
            
            # download
            out[a] = downloadFile(
              url = infile
              , destfile = destfile
              , method = opts$dlmethod
              , quiet = opts$quiet
            )
          }
          if (is.na(out[a])) {cat("File not found!\n"); unlink(destfile); break} # if NA then the url name is wrong!
          if (out[a]!=0 & !opts$quiet) {cat("Remote connection failed! Re-try:",g,"\r")} 
          if (out[a]==0 & !opts$quiet & g>1) {cat("Downloaded after:",g,"re-tries\n\n")}
          if (out[a]==0 & !opts$quiet & g==1) {cat("Downloaded by the first try!\n\n")}
          if (out[a]==0) {break}    
          Sys.sleep(opts$wait)
          g=g+1    
        }
    }
return(!as.logical(out)) 
}

doCheckIntegrity <- function(x, ...) {
  
  x <- basename(x)
  
  ## extract collection information
  clc = sapply(x, function(i) {
    prd = getProduct(i, quiet = TRUE)
    prd@CCC
  })

  opts <- combineOptions(...)
  
  opts$stubbornness <- stubborn(opts$stubbornness)
  
  out <- rep(NA,length=length(x))
  
  for (a in seq_along(x))
  { 
    if(basename(x[a])=="NA")
    {
      out[a] <- NA
    } else
    { 
      path <- do.call(genString, c(list(x = x[a], collection = clc[a]), opts))
      path$localPath <- setPath(path$localPath) 
      
      hv <- 1:length(path$remotePath)
      hv <- rep(hv,length=opts$stubbornness)
      g=1
      while(g <= opts$stubbornness) 
      {     
        if (g==1)
        {
          out[a] <- do.call(checkIntegrity, append(list(x = x[a]), opts))
        }
        
        if (is.na(out[a]))
        {
          unlink(x[a])
          break
        }
        if (!out[a])
        {
          if (!opts$quiet)
          {
            cat(basename(x[a]),"is corrupted, trying to re-download it!\n\n")
          }
          unlink(x[a])
          out[a] <- do.call(ModisFileDownloader, append(list(x = x[a]), opts))
        } else if (out[a]) 
        {
          break
        }
        
        out[a] <- do.call(checkIntegrity, append(list(x = x[a]), opts))
        g=g+1
      }
    }
  }
  return(as.logical(out)) 
}

# setPath for localArcPath and outDirPath
setPath <- function(path, ask=FALSE, showWarnings=FALSE, mkdir = TRUE)
{
  path <- normalizePath(path, "/", mustWork = FALSE)
  
  ##  Strip any trailing slashes from the path as file.exists() returns
  ##    FALSE for detecting folders with a trailing slash:
  path <- gsub("/$", "", path)
  
  if(!file.exists(path) & mkdir) 
  {
    doit <- 'Y'
    if (ask)
    {
      doit <- toupper(readline(paste0(path," does not exist, should it be created? [y/n]: ")))
    }
    
    if  (doit %in% c("Y","YES"))
    {
      stopifnot(dir.create(path, recursive = TRUE, showWarnings = showWarnings))
    } else
    {
      stop("Path not set, use ?MODISoptions to configure it")          
    }
  }
  return(correctPath(path))    
}

# get metadata
getMetadata = function(
  x
  , name
  , pattern
) {
  Filter(
    Negate(is.null)
    , Map(
      function(i) {
        tmp = sf::gdal_utils(source = i, quiet = TRUE)
        tmp = regmatches(tmp, regexpr(paste(name, pattern, sep = "="), tmp))
        
        if (length(tmp) > 0) {
          tmp = strsplit(tmp, "=|,")[[1]]
          as.numeric(tmp[2:length(tmp)])
        }
      }
      , x
    )
  )
}

# get NA values from getSds(x)$SDS4gdal
getNa = function(x) {
  getMetadata(
    stats::setNames(x, getSdsNames(x))
    , name = "NoData Value"
    , pattern = "[-]{0,1}\\d+"
  )
}

# get valid range as specified in hdf metadata: getSds(x)$SDS4gdal
getValidRange <- function(x) {
  getMetadata(
    stats::setNames(x, getSdsNames(x))
    , name = "valid_range"
    , pattern = "[-]{0,1}\\d+, \\d+"
  )
}

# If NA is within valid range mistrust
validNa <- function(x)
{
  na <- getNa(x)
  vr <- getValidRange(x)
  
  for(i in seq_along(vr))
  {
    #dt <- dataType(raster(x[i]))
    
    if(!is.na(vr[[i]][1]))
    {
      if(na[[i]] < min(vr[[i]]) | na[[i]] > max(vr[[i]][2]))
      {
        na[[i]] <- TRUE
      } else
      {
        na[[i]] <- FALSE
      }
    } else
    {
      na[[i]] <- NA
    }
  }
  return(na)
}

correctPath <- function(x,isFile=FALSE)
{
  if(!is.null(x))
  {  
    if (.Platform$OS.type=="windows")
    {
      x <- gsub(utils::shortPathName(normalizePath(x,winslash="/",mustWork=FALSE)),pattern="\\\\",replacement="/")
    } else
    {
      x <- path.expand(x)
    }
    if (substr(x,nchar(x),nchar(x))!="/" & !isFile)
    {
      x <- paste0(x,"/") 
    }
    x <- gsub(x,pattern="//",replacement="/")
  }
return(x)
}

positionIndication = function(x) {

  product = getProduct(x, quiet = TRUE)
  
  if (!is.null(product)) {
    ids = lapply(c("POS1", "POS2"), function(i) methods::slot(product, i))
    ids = do.call(rbind, ids)
    pos = list("POS1" = ids[1, ], "POS2" = ids[2, ])
    
    return(pos)
    
  } else {
    stop("Either provide position indications or input files conforming to "
         , "MODIS standard naming convention.\n")
  }
}

# For further information, see https://lpdaac.usgs.gov/dataset_discovery/modis.
getInfo = function(x, product = NULL, type = c("Tile", "CMG", "Swath")) {
  
  type = type[1]
  
  ## product short name (optional)
  if (is.null(product)) {
    product  <- sapply(strsplit(basename(x), "\\."), "[[", 1)
  }
  
  ## julian date of acquisition
  # stringr::str_extract(x, "A[:digit:]{7}")
  doa = regmatches(x, regexpr("A[[:digit:]]{7}", x))
  
  ## time of acquisition
  if (type == "Swath") {
    toa = regmatches(x, regexpr("\\.[[:digit:]]{4}\\.", x))
    toa = gsub("\\.", "", toa)
  }
  
  ## tile identifier
  tid = if (type == "Tile") {
    # stringr::str_extract(x, "h[0-3][0-9]v[0-1][0-9]")
    regmatches(x, regexpr("h[0-3][0-9]v[0-1][0-9]", x))
  } else "global"
  
  ## collection version
  # stringr::str_extract(x, "\\.[:digit:]{3}\\.")
  ccc = regmatches(x, regexpr("\\.[[:digit:]]{3}\\.", x))
  ccc = gsub("\\.", "", ccc)
  
  ## julian date of production
  # stringr::str_extract(x, "\\.[:digit:]{13}\\.")
  dop = regmatches(x, regexpr("\\.[[:digit:]]{13}\\.", x))
  dop = gsub("\\.", "", dop)
  
  ## data format
  # stringr::str_extract(x, "\\.[:alpha:]{2,3}$")
  fmt = regmatches(x, regexpr("\\.[[:alpha:]]{2,3}$", x))
  fmt = gsub("\\.", "", fmt)
  
  ## set list names and return
  out = list(product, doa, tid, ccc, dop, fmt)
  names(out) = c("PRODUCT", "DATE", if (type == "Swath") "TIME"
                 , if (type %in% c("Tile", "CMG")) "TILE", "CCC"
                 , "PROCESSINGDATE", "FORMAT")
  
  return(out)
}

## taken from https://cran.r-project.org/web/packages/maptools/vignettes/combine_maptools.pdf
fixOrphanedHoles = function(x) {
  polys <- slot(x, "polygons")
  fixed <- lapply(polys, maptools::checkPolygonsHoles)
  
  sp::SpatialPolygons(fixed, proj4string = sp::CRS(sp::proj4string(x)))
}

## skip unwanted products, see https://github.com/fdetsch/MODIS/issues/22
skipDuplicateProducts = function(x, quiet = FALSE) {
  
  products = as.character(getProduct()[, 1])
  
  dpl = sapply(products, function(i) {
    any(startsWith(products, i) & products != i)
  })
  
  if (any(names(dpl) == x) && dpl[names(dpl) == x]) {
    if (!quiet) {
      warning("Processing ", x, " only. Use regular expressions (eg. '"
              , x, ".*') to select more than one product.")
    }
    
    x = paste0("^", x, "$")
  }
  
  return(x)
}

## if required, reset 'begin' of composite product to corresponding release 
## date, see https://github.com/fdetsch/MODIS/issues/43
correctStartDate = function(begin, avDates, product, quiet = FALSE) {
  
  ## check if any older files exist  
  before = avDates < begin
  
  if (any(before) & !begin %in% avDates) {
    
    # if so, get date directly preceding 'begin'
    ids = which(before)
    cdt = avDates[length(ids)]
    
    # determine release cycle
    tbl = table(diff(avDates))
    rls = as.integer(names(tbl)[which.max(tbl)])
    
    # if time difference to preceding date is shorter than release cycle, 
    # reset 'begin' to this date
    if (difftime(begin, cdt, units = "days") < rls) {
      if (!quiet) {
        warning("Resetting 'begin' to start of corresponding ", product, " "
                , rls, "-day composite period (", cdt, ").")
      }
      
      begin = cdt
    }
  }
  
  return(begin)
}


### s2 ----

## see also https://github.com/fdetsch/MODIS/issues/110

modis_skip_s2 = function() {
  
  use_s2 = sf::sf_use_s2()
  
  ## disable use of s2 for spherical geometries
  jnk = modis_use_s2()
  
  return(
    use_s2
  )
}


modis_use_s2 = function(
  use_s2 = FALSE
) {
  
  ## omit console output from `sf::sf_use_s2()`
  jnk = suppressMessages(
    sf::sf_use_s2(
      use_s2
    )
  )
  
  return(
    invisible(
      jnk
    )
  )
}
