if ( !isGeneric("getHdf") ) {
  setGeneric("getHdf", function(HdfName, ...)
    standardGeneric("getHdf"))
}
#' Create or update a local subset of global online MODIS grid data pool
#' 
#' @description 
#' Create or update a local user-defined subset of the global MODIS grid data 
#' archive. Based on user-specific parameters the function checks in the local 
#' archive for available data and downloads missing data from the online MODIS 
#' data pool. When run in a schedule job, the function manage the continuous 
#' update of the local MODIS data archive.
#' 
#' @param product \code{character}. MODIS grid product to be downloaded, see 
#' \code{\link{getProduct}}. Use dot notation to address Terra and Aqua products 
#' (e.g. \code{M.D13Q1}). 
#' @param begin \code{character}. Begin date of MODIS time series, see 
#' \code{\link{transDate}} for formatting. 
#' @param end \code{character}. End date, compatible with future dates for 
#' continuous updates via scheduled jobs. 
#' @param tileH \code{numeric} or \code{character}. Horizontal tile number(s), 
#' see \code{\link{getTile}}.
#' @param tileV \code{numeric} or \code{character}. Vertical tile number(s), 
#' see \code{tileH}.
#' @param extent See Details in \code{\link{getTile}}.
#' @param collection \code{character} or \code{integer}. Desired MODIS product 
#' collection, see MODIS pages or \code{\link{getCollection}} for more information.
#' @param HdfName \code{character} vector or \code{list}. Full HDF file name(s) 
#' to download a small set of files. If specified, other file-related parameters 
#' (i.e., \code{begin}, \code{end}, \code{collection}, etc.) are ignored. 
#' @param wait \code{numeric}. Inserts a break (in seconds) after every internal 
#' call to \code{\link{download.file}} or \code{\link{getURL}}, which reduces 
#' the chance of FTP connection errors that frequently occur after many requests. 
#' @param checkIntegrity \code{logical}. If \code{TRUE} (default), the size of 
#' each downloaded file is checked. In case of inconsistencies, the function 
#' tries to re-download broken files. 
#' @param forceDownload \code{logical}. If \code{TRUE} (default), try to 
#' download data irrespective of whether online information could be retrieved 
#' via \code{MODIS:::getStruc} or not.
#' @param ... Arguments found in \code{\link{MODISoptions}}, sections 'STORAGE' 
#' and 'DOWNLOAD'.
#' 
#' @return 
#' An invisible vector of downloaded data and paths.
#' 
#' @references 
#' MODIS data is obtained through the online Data Pool at the NASA Land 
#' Processes Distributed Active Archive Center (LP DAAC), USGS/Earth Resources 
#' Observation and Science (EROS) Center, Sioux Falls, South Dakota 
#' \url{https://lpdaac.usgs.gov/get_data}.
#' 
#' SRTM data is obtained through CGIAR-CSI and mirror servers. For the use this 
#' data please read \url{http://srtm.csi.cgiar.org/SELECTION/SRT_disclaimer.htm}.
#' Jarvis A., H.I. Reuter, A.  Nelson, E. Guevara, 2008, Hole-filled seamless 
#' SRTM data V4, International Centre for Tropical Agriculture (CIAT), available 
#' from \url{http://srtm.csi.cgiar.org}.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' \dontrun{
#' # one or more specific file (no regular erpression allowed here)
#' a <- getHdf(c("MYD11A1.A2009001.h18v04.006.2015363221538.hdf", 
#'               "MYD11A1.A2009009.h18v04.006.2015364055036.hdf", 
#'               "MYD11A1.A2009017.h18v04.006.2015364115403.hdf"))
#' 
#' # DO NOT RUN THE NEXT EXAMPLE (unless you need this data)!!!
#' # Get all MODIS TERRA and AQUA 11A1 beginning from 18. October 2011 up to today.
#' # (Can be ran in a sceduled job, for daily archive update)
#' # getHdf(product="M.D11A1",begin="2011.10.18",tileH=18:19,tileV=4) 
#' 
#' # same Tiles with a LIST extent.
#' # approximatley 21 mB!
#' Austria <- list(xmax=17.47,xmin=9.2,ymin=46.12,ymax=49.3)
#' b <- getHdf(product="M.D11A1",begin="2009001",end="2009-01-02",extent=Austria)
#' b
#' 
#' # require(mapdata)
#' getHdf(product="M.D11A1",begin="2009.01.01",end="2009002",extent="austria")
#' # without specification of the extent... interactive selection see: "getTile()"
#' c <- getHdf(product="M.D11A1",begin="2009.01.01",end="2009002")
#' c
#' 
#' # SRTM data
#' # SRTM server limits downloads! (It seams to be blocked, but (normally) it continues after a while,
#' # you have to be patient!).
#' # The files are zips, this function only performs the download!
#' d <- getHdf(product="SRTM",extent="austria")  
#' d
#' }
#' 
#' @export getHdf
#' @name getHdf

################################################################################
### function using 'missing' input #############################################
#' @aliases getHdf,missing-method
#' @rdname getHdf
setMethod("getHdf",
          signature(HdfName = "missing"),
          function(product, 
                   begin=NULL, end=NULL, 
                   tileH=NULL, tileV=NULL, extent=NULL, 
                   collection=NULL, wait=0.5, checkIntegrity=TRUE,forceDownload=TRUE,...) 
{
  # product="MOD13Q1"; begin="2010001"; end="2010005"; tileH=NULL; tileV=NULL; extent=NULL; collection=NULL; wait=0.5; checkIntegrity=FALSE; z=1;u=1
  opts <- combineOptions(...)

  ## if 'quiet' is not available, show full console output
  if (!"quiet" %in% names(opts))
    opts$quiet <- FALSE

  sturheit <- stubborn(level=opts$stubbornness)
  wait     <- as.numeric(wait)

  # TODO HdfName as regex
  if (missing(product))
    stop("Please provide a supported 'product', see getProduct().\n")

  #######
  # check product
  product <- getProduct(x=product,quiet=TRUE)
  # check if missing collection, else bilieve it
  if(is.null(collection)) 
  {
    product$CCC <- getCollection(product=product,quiet=TRUE, forceCheck = TRUE)[[1]]
  } else
  {
      product$CCC <- sprintf("%03d",as.numeric(unlist(collection)[1]))
    }
    #########

    if (product$SENSOR[1]=="MODIS")
    {
      if (is.null(begin)) 
      {
        cat("No begin(-date) set, getting data from the beginning\n")
      } 
      if (is.null(end))
      {
        cat("No end(-date) set, getting data up to the most actual\n")
      } 

      # tranform dates
      tLimits <- transDate(begin=begin,end=end)

    } else if (product$SENSOR=="C-Band-RADAR")
    {
      if (!is.null(tileH) & !is.null(tileV))
      {   
        tileID <- getTile(tileH=tileH,tileV=tileV,system="SRTM")$tile
      } else 
      {
        tileID <- getTile(extent=extent,system="SRTM")$tile
      }
      ntiles <- length(tileID)
   
      ntiles <- length(tileID)
      path   <- genString("SRTM")
      files  <- paste0("srtm",tileID,".zip")
      dir.create(path$localPath,showWarnings=FALSE,recursive=TRUE)

      if (!file.exists(paste(path$localPath,"meta.zip",sep="/"))) 
      {
        cat("Getting SRTM metadata from: ftp://xftp.jrc.it\nThis is done once (the metadata is not used at the moment!)\n")
        download.file("ftp://xftp.jrc.it/pub/srtmV4/SRTM_META/meta.zip",paste(path$localPath,"meta.zip",sep="/"),
        mode='wb', method=opts$dlmethod, quiet=opts$quiet, cacheOK=TRUE)
      }
      if (!file.exists(paste(path$localPath,".SRTM_sizes",sep="/")))
      {
        sizes <- getURL(paste0(path$remotePath[[1]],"/"))
        sizes <- strsplit(sizes, if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]]
        sizes <- sapply(sizes,function(x){x <- strsplit(x," ")[[1]];paste(x[length(x)],x[length(x)-5],sep=" ")})
        names(sizes) <- NULL
        write.table(sizes,paste(path$localPath,".SRTM_sizes",sep="/"),quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
    
      sizes <- read.table(paste(path$localPath,".SRTM_sizes",sep="/"))

      files <- files[files %in% sizes[,1]] # remove Tiles that are not on the server

      startIND <- 1:length(path$remotePath) # for better cycling over the servers
      startIND <- rep(startIND,length(files))

      cat("Be avare, that sources for SRTM data have limited the number of requests!\nNormally it suspends the download, and after a while it continues. So may you have to be patient!\n")

    for(d in seq_along(files)) 
    {
      isOK <- TRUE
      if (file.exists(paste0(path$localPath,"/",files[d])))
      {
       isOK <- checksizefun(file=paste0(path$localPath,"/",files[d]),sizeInfo=sizes,flexB=50000)$isOK # flexB!
      }
      if (!file.exists(paste0(path$localPath,"/",files[d]))| !isOK)
      {
        timeout <- options("timeout") # TEST I'm not sure if it helps (timeout is used in ?download.file)
        options(timeout=15)

        for(g in 1:sturheit) 
        {
          server <- names(path$remotePath)[rep(startIND[d:(d+length(path$remotePath)-1)],length=sturheit)]
          cat("Getting SRTM data from:",server[g],"\n")
          Sys.sleep(wait)        
                          
          hdf=1
          try(
              hdf <- download.file(
                  paste0(path$remotePath[[server[g]]],"/", files[d]),
                  destfile=paste0(path$localPath,"/", files[d]),
                  mode='wb', method=opts$dlmethod, quiet=opts$quiet, cacheOK=TRUE),
              silent=TRUE
          )
          if (hdf==0) 
          {
            SizeCheck <- checksizefun(file=paste0(path$localPath,"/", files[d]),sizeInfo=sizes,flexB=50000)
            if(!SizeCheck$isOK) {hdf=1} # if size check fails, re-try!
          }
          if(hdf==0 & !opts$quiet) 
          {
            lastused <- server[g] 
            if (g==1) 
            {
              cat("Downloaded by the first try!\n\n")
            } else 
            {
              cat("Downloaded after",g,"retries!\n\n")
            }
          }
          if(hdf==0) 
          {
            break
          }    
        }
      options(timeout=as.numeric(timeout)) # set timeout back to default
      }
    }
    SRTM <- paste0(path$localPath,"/",files)
    return(invisible(SRTM))
  }
  
  dates  <- list()
  output <- list() # path info for the invisible output
  l=0
       
  for(z in seq_along(product$PRODUCT))
  { # Platforms MOD/MYD

    if (product$TYPE[z]=="Swath") 
    {
        cat("'Swath'-products not yet supported, jumping to the next.\n")
    } else 
    {
      todo <- paste0(product$PRODUCT[z],".",product$CCC)
      for (u in seq_along(todo))
      {
        # tileID
        if (product$TYPE[z]=="CMG") 
        {
          tileID="GLOBAL"
          ntiles=1 
        } else 
        {
          if (!is.null(tileH) & !is.null(tileV)) 
          {
            extent <- getTile(tileH=tileH,tileV=tileV)
          } else
          {
            extent <- getTile(extent=extent)
          }
          tileID <- extent$tile
          ntiles <- length(tileID)
        }
        
        ## ensure compatibility with servers other than those specified in 
        ## `opts$MODISserverOrder`, e.g. when downloading 'MOD16A2' from NTSG
        server <- unique(unlist(product$SOURCE))
        
        if (length(server) > 1) {
          # alternative server, i.e. when priority is not reachable
          server_alt <- server[which(server != opts$MODISserverOrder[1])]
          # priority server from which structure will be tried to retrieve first
          server <- server[which(server == opts$MODISserverOrder[1])]
        }
          
        ## this time, suppress console output from `getStruc`
        jnk <- capture.output(
          onlineInfo <- getStruc(product = product$PRODUCT[z], server = server, 
                                 collection = product$CCC, begin = tLimits$begin, 
                                 end = tLimits$end, wait = 0)
        )
        
        if(!is.na(onlineInfo$online))
        {
          if (!onlineInfo$online & length(opts$MODISserverOrder)==2 & 
              server %in% c("LPDAAC", "LAADS"))
          {
            cat(server," seams not online, trying on '",server_alt,"':\n",sep="")
            onlineInfo <- getStruc(product = product$PRODUCT[z], collection = product$CCC,
                                   begin = tLimits$begin, end = tLimits$end, 
                                   wait = 0, server = server_alt)
          }
          if(is.null(onlineInfo$dates))
          {
            stop("Could not connect to server(s), and no data is available offline!\n")
          }
          if(!is.na(onlineInfo$online))
          {
            if(!onlineInfo$online & !forceDownload)
            {
              cat("Could not connect to server(s), data download disabled! Try to set 'forceDownload = TRUE' in order to enable online file download...\n")
            }
          }
        }
        
        datedirs <- as.Date(onlineInfo$dates)
        datedirs <- datedirs[!is.na(datedirs)]            
        sel <- datedirs
        us  <- sel >= tLimits$begin & sel <= tLimits$end
        
        if (sum(us,na.rm=TRUE)>0)
        { 
          suboutput <- list()
          l=l+1                
          dates[[l]] <- datedirs[us]

          dates[[l]] <- cbind(as.character(dates[[l]]),matrix(rep(NA, length(dates[[l]])*ntiles),ncol=ntiles,nrow=length(dates[[l]])))
          colnames(dates[[l]]) <- c("date",tileID)

          for (i in 1:nrow(dates[[l]]))
          { # i=1
            #cat(dates[[l]][i,1],"\n")
            #flush.console()
            
            year <- format(as.Date(dates[[l]][i,1]), "%Y")
            doy  <- as.integer(format(as.Date(dates[[l]][i,1]), "%j"))
            doy  <- sprintf("%03d",doy)
            mtr  <- rep(1,ntiles) # for file availability flaging
            path <- genString(x=strsplit(todo[u],"\\.")[[1]][1],collection=strsplit(todo[u],"\\.")[[1]][2],date=dates[[l]][i,1])
            
            for(j in 1:ntiles)
            {  
              dates[[l]][i,j+1] <- paste0(strsplit(todo[u],"\\.")[[1]][1],".",paste0("A",year,doy),".",if (tileID[j]!="GLOBAL") {paste0(tileID[j],".")},strsplit(todo[u],"\\.")[[1]][2],".*.hdf$") # create pattern            
              if (length(dir(path$localPath,pattern=dates[[l]][i,j+1]))>0)
              { # if available locally
                HDF <- dir(path$localPath,pattern=dates[[l]][i,j+1]) # extract HDF file

                if (length(HDF)>1)
                { # in very recent files sometimes there is more than 1 file/tile/date if so get the most recent processing date
                  select <- list()
                  for (d in 1:length(HDF))
                  { 
                      select[[d]]<- strsplit(HDF[d],"\\.")[[1]][5]
                  }
                  HDF <- HDF[which.max(unlist(select))]        
                }
                dates[[l]][i,j+1] <- HDF
                mtr[j] <- 0
              }
            }
            
            if (sum(mtr)!=0 & (onlineInfo$online | is.na(onlineInfo$online) | forceDownload)) 
            { # if one or more of the tiles in the given date is missing, its necessary to go online

              if(exists("ftpfiles")) 
              {
                  rm(ftpfiles)
              }
              
              for (g in 1:sturheit)
              { # get list of FILES in remote dir
  #                        server <- names(path$remotePath)[g%%length(path$remotePath)+1]
                ftpfiles <- try(filesUrl(path$remotePath[[which(names(path$remotePath)==onlineInfo$source)]]),silent=TRUE)
                
                if(ftpfiles[1]==FALSE)
                {
                  rm(ftpfiles)
                }
                if(exists("ftpfiles"))
                {
                  break
                }
                Sys.sleep(wait)
              }
              
              if(!exists("ftpfiles")) 
              {
                stop("Problems with online connections try a little later")
              }
              
              if (ftpfiles[1] != "total 0") 
              {
                ftpfiles <- unlist(lapply(strsplit(ftpfiles," "),function(x){x[length(x)]})) # found empty dir!

                for(j in 1:ntiles)
                { # j=1
                  if(mtr[j]==1)
                  { # if tile is missing get it
                    onFtp <- grep(ftpfiles,pattern=dates[[l]][i,j+1],value=TRUE)
                    HDF   <- grep(onFtp,pattern=".hdf$",value=TRUE)

                    if(length(HDF)>0)
                    {
                      if (length(HDF)>1) 
                      { # in very recent files sometimes there is more than 1 file/tile/date if so get the last
                        select <- list()
                        for (d in seq_along(HDF))
                        {
                          select[[d]] <- strsplit(HDF[d],"\\.")[[1]][5]
                        }
                        HDF <- HDF[which.max(unlist(select))]        
                      }

                      dates[[l]][i,j+1] <- HDF
                      hdf <- ModisFileDownloader(HDF, wait=wait, quiet=opts$quiet)
                      mtr[j] <- hdf

                    } else 
                    { 
                      dates[[l]][i,j+1] <- NA 
                    }
                  }
                }
              } else 
              {
                dates[[l]][i,(j+1):ncol(dates[[l]])] <- NA
              } # on ftp is possible to find empty folders!
            }
            if(checkIntegrity)
            { # after each 'i' do the sizeCheck
              isIn <- doCheckIntegrity(paste0(path$localPath,dates[[l]][i,-1]), wait=wait, quiet=opts$quiet,...)
            }
          suboutput[[i]] <- paste0(path$localPath,dates[[l]][i,-1])                    
          } # end i

          output[[l]] <-  as.character(unlist(suboutput))
          names(output)[l] <- todo[u]
        } else 
        {
          cat(paste0("No files on ftp in date range for: ",todo[u],"\n\n"))
        }
      } 
    }
    }
    return(invisible(output))
}) ## END: FTP vs ARC check and download 


################################################################################
### function using 'character' input ###########################################
#' @aliases getHdf,character-method
#' @rdname getHdf
setMethod("getHdf",
          signature(HdfName = "character"),
          function(HdfName, wait = 0.5, checkIntegrity = TRUE, ...) {
            
  opts <- combineOptions(...)
            
  ## if 'quiet' is not available, show full console output
  if (!"quiet" %in% names(opts))
    opts$quiet <- FALSE
  
  wait <- as.numeric(wait)
  
  ## loop over 'HdfName'
  HdfName <- basename(HdfName)  
  
  dates <- sapply(HdfName, function(i) {

    path <- genString(i, opts = opts)
    path$localPath <- setPath(path$localPath)
    
    if (!file.exists(paste0(path$localPath, "/", i))) 
      ModisFileDownloader(i, wait = wait, opts = opts)

    if(checkIntegrity)
      jnk <- doCheckIntegrity(i, wait = wait, opts = opts)

    paste0(path$local, "/", i)
  })
  
  ## return output
  return(invisible(dates))
})


################################################################################
### function using 'list' input ################################################
#' @aliases getHdf,list-method
#' @rdname getHdf
setMethod("getHdf",
          signature(HdfName = "list"),
          function(HdfName, wait = 0.5, checkIntegrity = TRUE, ...) {
            
  ## unlist 'HdfName' and call 'character' method
  HdfName <- unlist(HdfName)
  
  getHdf(HdfName, wait, checkIntegrity, ...)
})