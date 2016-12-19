#' Minor MODIS Package Functions
#'
#' @description 
#' Compendium of minor \strong{MODIS} package-related functions.
#' 
#' @param pattern Regular expression passed to \code{\link{grep}}.
#' @param database \code{character}. Defaults to \code{"worldHires"}, see 
#' \code{\link{map}} for available options.
#' @param plot \code{logical}, defaults to \code{FALSE}. If \code{TRUE}, search 
#' results are displayed.
#' 
#' @return 
#' A \code{list} of length 2. The first entry is the call to create the given 
#' map, whereas the second entry represents the names of areas within the 
#' search. 
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @seealso 
#' \code{\link{getTile}}, \code{\link{map}}, \code{\link{grep}}.
#' 
#' @examples 
#' \dontrun{
#' search4map()
#' 
#' search4map(pattern="USA",plot=TRUE)
#' search4map(database="state",plot=TRUE)?map
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


checksizefun <- function(file,sizeInfo=NULL,flexB=0)
{
    # determine reference size
    if (is.null(sizeInfo))
    {
        xmlfile  <- paste0(file,".xml")
        xmlfile  <- xmlParse(xmlfile)
        MetaSize <- getNodeSet(xmlfile, "/GranuleMetaDataFile/GranuleURMetaData/DataFiles/DataFileContainer/FileSize" )
        MetaSize <- as.numeric(xmlValue(MetaSize[[1]])) # expected filesize
    } else 
    {
        MetaSize <- as.numeric(sizeInfo[which(sizeInfo[,1]==basename(file)),2])
    }
    
    if(length(MetaSize)==0)
    {
        res  <- list(MetaSize=NULL,FileSize=NULL,isOK=NULL)
        return(res)
    }
    
    FileSize <- as.numeric(fileSize(file))
    if (flexB!=0)
    {
        isOK <- (MetaSize >= FileSize-flexB & MetaSize <= FileSize+flexB)
    } else 
    {
        isOK <- (MetaSize == FileSize)
    }
    res  <- list(MetaSize=MetaSize,FileSize=FileSize,isOK=as.logical(isOK))
return(res)
}


#' @describeIn minorFuns Simplifies search for \strong{mapdata}-based extents
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

checkTools <- function(tool=c("MRT","GDAL"), quiet=FALSE, opts = NULL)
{
    tool <- toupper(tool)
    
    iw   <- options()$warn 
    options(warn=-1)
    on.exit(options(warn=iw))
    
    MRT  <- NULL
    GDAL <- NULL
    
    if ("MRT" %in% tool)
    {
        MRT   <- FALSE
        mrtH  <- normalizePath(Sys.getenv("MRT_HOME"), winslash="/", mustWork = FALSE)
        mrtDD <- normalizePath(Sys.getenv("MRT_DATA_DIR"), winslash="/", mustWork = FALSE)
        
        if (!quiet)
        {
            cat("Checking availabillity of MRT:\n")
        }
    
        if(mrtH=="") 
        {
            cat("  'MRT_HOME' not set/found! MRT is NOT enabled! See: 'https://lpdaac.usgs.gov/tools/modis_reprojection_tool'\n")
        } else 
        {
            if (!quiet)
            {
                cat("  'MRT_HOME' found:", mrtH,"\n")
            }
            if (mrtDD=="") 
            {
               cat("  'MRT_DATA_DIR' not set/found! MRT is NOT enabled! You need to set the path, read in the MRT manual! 'https://lpdaac.usgs.gov/tools/modis_reprojection_tool'\n")
            } else 
            {
                if (!quiet)
                {
                    cat("  'MRT_DATA_DIR' found:",mrtDD,"\n")
                    cat("   MRT enabled, settings are fine!\n")
                }
                MRT <- TRUE
            } 
        }
        if(MRT)
        {
            if(file.exists(paste0(mrtH,"/doc/ReleaseNotes.txt")))
            {
              x <- file(paste0(mrtH,"/doc/ReleaseNotes.txt"),open="rt")
              v <- readLines(x)
              v <- v[(grep(v,pattern="------*")-1)]
              v <- v[grep(v,pattern="Version ")][1]
              close(x)
            } else
            {
              v <- "Enabled"
            }
        } else 
        {
            v <- "Version not determined"
        }
        MRT <- list(MRT=MRT,version=v)
    }

    if ("GDAL" %in% tool)
    {
        GDAL <- FALSE
        gdv  <- NA
        
        if (is.null(opts))
          opts <- combineOptions(checkTools = FALSE)
        
        if (.Platform$OS=="unix")
        {    
            if (!quiet)
            {
                cat("Checking availabillity of GDAL:\n")
            }
            
            cmd      <- paste0(opts$gdalPath,'gdalinfo --version')            
            gdaltext <- try(system(cmd,intern=TRUE),silent=TRUE)
                         
            if (inherits(gdaltext,"try-error"))
            {
                cat("   GDAL not found, install 'gdal-bin' or check path settings in order to use related functionalities (see '?MODISoptions')!\n")
                gdaltext <- "Could not determine GDAL version!"
            } else 
            {
                if (!quiet)
                {
                    cat("   OK,",gdaltext,"found!\n")
                }
                GDAL <- TRUE
                
                gdv <- strsplit(gdaltext,",")[[1]][1]
                gdv <- raster::trim(gsub(gdv,pattern="GDAL",replacement=""))
                gdv <- as.numeric(strsplit(gdv,"\\.")[[1]])
            }
            GDAL <- list(GDAL=GDAL,version=gdaltext,vercheck=gdv)
            
        } else 
        {
            if (!quiet)
            {
                cat("Checking availability of 'FWTools/OSGeo4W' (GDAL with HDF4 support for Windows):\n")    
            }
            
            cmd <- paste0(opts$gdalPath,'gdalinfo --version')            
            
            gdaltext <- shell(cmd,intern=TRUE)
            
            if (length(grep(x=gdaltext,pattern="GDAL",ignore.case = TRUE))==0)
            {
                cat("'FWTools/OSGeo4W' installation not found or path not set.\nIf you don't have installed one of them you can get it from 'http://fwtools.maptools.org/' or 'http://trac.osgeo.org/osgeo4w/' (recommanded)\nTrying to autodetect path to 'FWTools/OSGeo4W' (this may takes some time, you can interupt this process and set it manually, see 'gdalPath' argument in '?MODISoptions':\n\n")
                
                a <- dirname(list.files(path="c:/",pattern="^gdalinfo.exe$", full.names=TRUE, recursive=TRUE,include.dirs=TRUE))

                if (length(a)==0)
                {
                    stop("No 'FWTools/OSGeo4W' installation(s) found! In order to use related function please solve this problem first.\n")
                }

                fwt <- a[grep(a,pattern="FWTools",ignore.case = TRUE)]
                osg <- a[grep(a,pattern="OSGeo4W",ignore.case = TRUE)]
                minone <- FALSE
                if(length(fwt)==1)
                {
                    fwtP <- shQuote(utils::shortPathName(normalizePath(paste0(fwt,"/gdalinfo.exe"),winslash="/")))
                    fwtV <- shell(paste0(fwtP, " --version"),intern=TRUE)
                    fwtV <- strsplit(strsplit(fwtV,",")[[1]][1]," ")[[1]][2]
                  
                    if(checkGdalDriver(fwt))
                    {                  
                        cat("Found 'FWTools' version: '", fwtV,"' to enalbe this run:\n MODISoptions(gdalPath='",normalizePath(fwt,"/"),"')\n",sep="")
                        minone <- TRUE
                    } else 
                    {
                        cat("Found 'FWTools' version: '", fwtV,"' in '",normalizePath(fwt,"/"),"' but without HDF4 support...strange, try to remove and re-install 'FWTools'!\n",sep="")
                    }
                }
                if(length(osg)==1)
                {
                    osgP <- shQuote(utils::shortPathName(normalizePath(paste0(osg,"/gdalinfo.exe"),winslash="/")))
                    osgV <- shell(paste0(osgP, " --version"),intern=TRUE)
                    osgV <- strsplit(strsplit(osgV,",")[[1]][1]," ")[[1]][2]
                  
                    if(checkGdalDriver(osg))
                    {                  
                        cat("Found 'OSgeo4W' version: '", osgV,"' to enable this run:\n MODISoptions(gdalPath='",normalizePath(osg,"/"),"')\n",sep="")
                        minone <- TRUE
                    } else 
                    {
                        cat("Found 'OSgeo4W' version: '", osgV,"' in '",normalizePath(osg,"/"),"' but without HDF4 support...strange, try to remove and re-install 'OSgeo4W'!\n",sep="")
                    }
                }
                if (!minone)
                {
                    cat("No HDF4 supporting GDAL installation found. You may set it manually in MODISoptions(gdalPath='/Path/to/XXGDAL/bin')\n")
                }
                gdaltext <- "Could not determine GDAL version!"

            } else 
            {
                if (!quiet)
                {
                    cat("   OK,",gdaltext,"found!\n")
                }
                GDAL <- TRUE
                gdv <- strsplit(gdaltext,",")[[1]][1]
                gdv <- raster::trim(gsub(gdv,pattern="GDAL",replacement=""))
                gdv <- as.numeric(strsplit(gdv,"\\.")[[1]])

            }
            GDAL <- list(GDAL = GDAL, version = gdaltext,vercheck=gdv)
        }
    }
    return(invisible(list(GDAL=GDAL,MRT=MRT)))        
}


# get gdal write formats (driver 'name', 'long name' and 'extension')
gdalWriteDriver <- function(renew = FALSE, quiet = TRUE,...)
{
  iw   <- options()$warn 
  options(warn=-1)
  on.exit(options(warn=iw))

  opt <- combineOptions(...)
     
  outfile <- paste0(opt$outDirPath,".auxiliaries/gdalOutDriver.RData")
  
  if (!is.null(getOption("MODIS_gdalOutDriver"))) # take it from options()
  {
    gdalOutDriver <- getOption("MODIS_gdalOutDriver")
  } else if(file.exists(outfile)) # or from RData
  {
    load(outfile)
  }  
  
  if(exists("gdalOutDriver"))
  {
    if (nrow(gdalOutDriver)<5)
    {
      renew <- TRUE
    }
  } else
  {
    renew <- TRUE
  }

  if (renew)
  {
    if(!quiet)
    {
      message("Detecting available write drivers!")
    }
    
    cmd <- paste0(opt$gdalPath,"gdalinfo --formats")
    
    # list all drivers with (rw)
    if (.Platform$OS=="unix")
    {
      gdalOutDriver <- system(cmd,intern=TRUE)
    } else
    {
      gdalOutDriver <- shell(cmd,intern=TRUE)
    }
    
    gdalOutDriver <- grep(gdalOutDriver,pattern="\\(rw",value=TRUE) # this regex must be preciser
    name          <- sapply(gdalOutDriver,function(x){strsplit(x,"\\(")[[1]][1]})
    name          <- gsub(as.character(name), pattern=" ", replacement="")
    #tnauss
    name          <- sapply(name, function(x){return(strsplit(x, "-")[[1]][1])})
    
    description <- as.character(sapply(gdalOutDriver,function(x){strsplit(x,"\\): ")[[1]][2]}))
    
    if(!quiet)
    {
      message("Found: ",length(name)," candidate drivers, detecting file extensions...")
    }
    
    extension <- rep(NA,length(name))
    for (i in seq_along(name))
    {
      ind <- grep(name, pattern=paste0("^",name[i],"$"), ignore.case=TRUE, value=FALSE)
      
      if (length(ind)!=0)
      {
        extension[i] <- getExtension(name[ind],gdalPath = opt$gdalPath)
      }
    }
    if(!quiet)
    {
      message(sum(!is.na(extension))," usable drivers detected!")
    }
    gdalOutDriver <- data.frame(name=name[!is.na(extension)], description=description[!is.na(extension)], extension=extension[!is.na(extension)], stringsAsFactors=FALSE)        
    
    if(!file.exists(opt$outDirPath))
    {
	    opt$outDirPath <- setPath(opt$outDirPath,ask = TRUE)
      opt$auxPath    <- setPath(paste0(opt$outDirPath,".auxiliaries"),ask=FALSE)
    }
    
    if(file.exists(opt$auxPath))
    {
      save(gdalOutDriver, file=outfile)
    }
  }
  gdalOutDriver
}


getExtension <- function(dataFormat,...)
{
  if(toupper(dataFormat) %in% c("HDF-EOS","HDF4IMAGE")) # MRT + GDAL
  {
    return(".hdf")
  } else if (toupper(dataFormat) %in% c("GTIFF","GEOTIFF"))  # MRT + GDAL
  {
    return(".tif")
  } else if (tolower(dataFormat) =="raw binary")  # MRT + GDAL
  {
    return(".hdr")
  } else if (toupper(dataFormat)=="ENVI") 
  {
    return("") # should generate a '.hdr' file + a file without extension
  } else if (dataFormat=="FIT") 
  {
    return(NA)    
  } else if (toupper(dataFormat)=="ILWIS")
  {
    return(".mpr") # is this ok?
  } else 
  {
    gdalPath <- combineOptions(...)$gdalPath
    cmd <- paste0(gdalPath,'gdalinfo --format ')
    
    if(.Platform$OS.type=="unix")
    {
      ext <- system(paste0(cmd, dataFormat),intern=TRUE)   
    } else
    {
      ext <- shell(paste0(cmd, dataFormat),intern=TRUE)   
    }
    
    ext <- grep(ext,pattern="Extension:",value=TRUE)
    
    if(length(ext)==0)
    {
      return(NA)
    } else
    {
      ext <- gsub(strsplit(ext,":")[[1]][2],pattern=" ",replacement="")
      
      if (ext!="")
      {
        ext <- paste0(".",ext)
      }
      return(ext)
    }
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
      secName <- defineName(product$request)
      
      if (product$SENSOR[1] == "MODIS") 
      {
        if (product$TYPE[1] == "Tile") 
        {
          Tpat    <- "h[0-3][0-9]v[0-1][0-9]" # to enhance
          return(all((grep(secName["TILE"],pattern=Tpat)) + (substr(secName["DATE"],1,1) == "A") + (length(secName)==6)))
          
        } else if (product$TYPE[1] == "CMG") 
        {
          return(all((substr(secName["DATE"],1,1) == "A") + (length(secName)==5)))
          
        } else if (product$TYPE[1] == "Swath")  # actually no support for Swath data!
        {
#             return(all((substr(secName["DATE"],1,1) == "A") + (length(secName)==6)))
#                } else {
          return(FALSE)
        }
      } else 
      {
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
      if (product$TYPE=="Tile") 
      {
        names(secName) <- c("PRODUCT","DATE","TILE","CCC","PROCESSINGDATE","FORMAT")
      } else if (product$TYPE=="CMG") 
      {
        names(secName) <- c("PRODUCT","DATE","CCC","PROCESSINGDATE","FORMAT")
      } else if (product$TYPE=="Swath") 
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
    
    iw   <- options()$warn 
    options(warn=-1)
    on.exit(options(warn=iw))

    ## default method (e.g. LPDAAC, LAADS)
    if (length(grep("ntsg", url)) == 0) {
      
      try(co <- RCurl::getURLContent(url),silent=TRUE)
      
      if (!exists("co")) {return(FALSE)}
      
      if (substring(url,1,4)=="http")
      {
        co     <- htmlTreeParse(co)
        co     <- co$children[[1]][[2]][[2]]
        co     <- sapply(co$children, function(el) xmlGetAttr(el, "href"))
        co     <- as.character(unlist(co))
        co     <- co[!co %in% c("?C=N;O=D", "?C=M;O=A", "?C=S;O=A", "?C=D;O=A")]
        fnames <- co[-1] 
        
      } else 
      {
        co <- strsplit(co, if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]]
        
        co   <- strsplit(co," ")
        elim <- grep(co,pattern="total")
        if(length(elim)==1)
        {
          co <- co[-elim]
        }
        fnames <- basename(sapply(co,function(x){x[length(x)]}))
      }
      
    ## NTSG method; if not used, connection breakdowns are likely to occur  
    } else {
      
      # 'MODIS' options
      opts <- combineOptions()
      
      # download website to opts$auxPath
      file_out_dn <- opts$auxPath
      file_out_bn <- unlist(strsplit(url, "/"))
      file_out <- paste(file_out_dn, file_out_bn[length(file_out_bn)], sep = "/")
      download.file(url = url, destfile = file_out, quiet = TRUE, method = "libcurl")
      
      # extract information from website content
      content <- readLines(file_out)
      
      fnames <- sapply(content, function(i) {
        unlist(strsplit(i, " "))[length(unlist(strsplit(i, " ")))]
      })
      names(fnames) <- NULL

      # remove temporary file and return output
      invisible(file.remove(file_out))
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
ModisFileDownloader <- function(x, wait = 0.5, opts = NULL, ...)
{
    x <- basename(x)

    ## if options have not been passed down, create them from '...'
    if (is.null(opts))
      opts <- combineOptions(...)
    
    opts$stubbornness <- stubborn(opts$stubbornness)

    ## if 'quiet' is not available, show full console output
    if (!"quiet" %in% names(opts))
      opts$quiet <- FALSE
    
    iw <- options()$warn 
    options(warn=-1)
    on.exit(options(warn=iw))

    out <- rep(NA,length=length(x))
    
    for (a in seq_along(x))
    {  # a=1
        path <- genString(x[a], opts = opts)
        path$localPath <- setPath(path$localPath)
        
        hv <- seq_along(opts$MODISserverOrder)
        hv <- rep(hv,length=opts$stubbornness)
        g=1
        while(g <= opts$stubbornness) 
        {     
          if (!opts$quiet)
          {
              cat("\nGetting file from:",opts$MODISserverOrder[hv[g]],"\n############################\n")
          }
          destfile <- paste0(path$localPath,x[a])
          
          if(!.Platform$OS=="windows" & opts$dlmethod=="aria2")
          {
            out[a] <- system(paste0("aria2c -x 3 --file-allocation=none ",paste(path$remotePath[which(names(path$remotePath)==opts$MODISserverOrder[hv[g]])],x[a],sep="/",collapse="")," -d ", dirname(destfile)))
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
            if (length(server) > 1)
              server <- server[which(server %in% opts$MODISserverOrder[hv[g]])]
              
            infile <- paste(path$remotePath[id_remotepath], x[a], sep = "/", 
                            collapse = "")
            
            ## adapt 'dlmethod' and 'extra' if server == "LPDAAC"
            if (server == "LPDAAC") {
              if (!opts$dlmethod %in% c("wget", "curl")) {
                warning("Data download from '", server, 
                        "' is currently only available through wget and curl.\n", 
                        "Setting MODISoptions(dlmethod = 'wget') ",  
                        "(or run MODISoptions(dlmethod = 'curl') to use curl instead) ...\n")
                method <- "wget"
              } else {
                method <- opts$dlmethod
              }
              
              # wget extras
              extra <- if (method == "wget") {
                paste("--load-cookies ~/.cookies.txt", 
                      "--save-cookies ~/.cookies.txt --keep-session-cookie", 
                      "--no-check-certificate")
              # curl extras  
              } else {
                '-n -L -c ~/.cookies.txt -b ~/.cookies.txt'
              }
              
            ## else use default settings
            } else {
              method <- opts$dlmethod
              extra <- getOption("download.file.extra")
            }
            
            out[a] <- try(
              download.file(url = infile, destfile = destfile, mode = 'wb', 
                            method = method, quiet = opts$quiet, 
                            cacheOK = FALSE, extra = extra),
                          silent = TRUE)
          }
          if (is.na(out[a])) {cat("File not found!\n"); unlink(destfile); break} # if NA then the url name is wrong!
          if (out[a]!=0 & !opts$quiet) {cat("Remote connection failed! Re-try:",g,"\r")} 
          if (out[a]==0 & !opts$quiet & g>1) {cat("Downloaded after:",g,"re-tries\n\n")}
          if (out[a]==0 & !opts$quiet & g==1) {cat("Downloaded by the first try!\n\n")}
          if (out[a]==0) {break}    
          Sys.sleep(wait)
          g=g+1    
        }
    }
return(!as.logical(out)) 
}

doCheckIntegrity <- function(x, wait = 0.5, opts = NULL, ...) {
  
  x <- basename(x)
  
  ## if options have not been passed down, create them from '...'
  if (is.null(opts))
    opts <- combineOptions(...)
  
  opts$stubbornness <- stubborn(opts$stubbornness)
  
  ## if 'quiet' is not available, show full console output
  if (!"quiet" %in% names(opts))
    opts$quiet <- FALSE
  
  out <- rep(NA,length=length(x))
  
  for (a in seq_along(x))
  { 
    if(basename(x[a])=="NA")
    {
      out[a] <- NA
    } else
    { 
      path <- genString(x[a], opts = opts)
      path$localPath <- setPath(path$localPath) 
      
      hv <- 1:length(path$remotePath)
      hv <- rep(hv,length=opts$stubbornness)
      g=1
      while(g <= opts$stubbornness) 
      {     
        if (g==1)
        {
          out[a] <- checkIntegrity(x = x[a], opts = opts)
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
          out[a] <- ModisFileDownloader(x[a], wait = wait, opts = opts)
        } else if (out[a]) 
        {
          break
        }
        
        out[a] <- checkIntegrity(x = x[a], opts = opts)
        g=g+1
      }
    }
  }
  return(as.logical(out)) 
}

# setPath for localArcPath and outDirPath
setPath <- function(path, ask=FALSE, showWarnings=FALSE)
{
  path <- normalizePath(path, "/", mustWork = FALSE)
  
  ##  Strip any trailing slashes from the path as file.exists() returns
  ##    FALSE for detecting folders with a trailing slash:
  path <- gsub("/$", "", path)
  
  if(!file.exists(path)) 
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

# get NA values from getSds(x)$SDS4gdal
getNa <- function(x)
{
  name <- res <- vector(mode="list",length=length(x))
  
  iw   <- getOption("warn") 
  options(warn=-1)
  on.exit(options(warn=iw))

  gdalPath <- getOption("MODIS_gdalPath")[1]
  gdalPath <- correctPath(gdalPath)
  cmd <- paste0(gdalPath,"gdalinfo ")
  
  for (i in seq_along(x))
  {
    tmp    <- system(paste0(cmd,shQuote(x[i])),intern=TRUE)
    tmp    <- grep(tmp,pattern="NoData Value=",value=TRUE)
    if (length(tmp)!=0)
    {
        res[[i]] <- as.numeric(strsplit(tmp,"=")[[1]][2])
    } else
    {
        res[[i]] <- NA
    }
    nam       <- strsplit(x[i],":")[[1]] 
    name[[i]] <- nam[length(nam)]
  }
  
  names(res) <- unlist(name)
  res[is.na(res)] <- NULL
  return(res)
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
