if ( !isGeneric("getHdf") ) {
  setGeneric("getHdf", function(product, ...)
    standardGeneric("getHdf"))
}
#' Create or Update Local Subset of Online MODIS Data Pool
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
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' \dontrun{
#' # One or more specific file (no regular erpression allowed here)
#' a <- getHdf(HdfName = c("MYD11A1.A2009001.h18v04.006.2015363221538.hdf", 
#'                         "MYD11A1.A2009009.h18v04.006.2015364055036.hdf", 
#'                         "MYD11A1.A2009017.h18v04.006.2015364115403.hdf"))
#' a
#' 
#' # Get all MODIS Terra and Aqua M*D11A1 data from 1 December 2016 up to today
#' # (can be ran in a sceduled job for daily archive update)
#' b1 <- getHdf(product = "M.D11A1", begin = "2016.12.01", 
#'              tileH = 18:19, tileV = 4) 
#' b1               
#' 
#' # Same tiles with a 'list' extent
#' Austria <- list(xmax = 17.47, xmin = 9.2, ymin = 46.12, ymax = 49.3)
#' b2 <- getHdf(product = "M.D11A1", begin = "2016336", extent = Austria)
#' b2
#' 
#' # Using country boarders from 'mapdata' package
#' c <- getHdf(product = "M.D11A1", begin = "2016306", end = "2016335",
#'             extent = "Luxembourg")
#' c             
#'        
#' # Interactive selection of spatial extent, see getTile()
#' d <- getHdf(product = "M.D11A1", begin = "2016306", end = "2016307")
#' d
#' }
#' 
#' @export getHdf
#' @name getHdf

################################################################################
### function using 'character' input ###########################################
#' @aliases getHdf,character-method
#' @rdname getHdf
setMethod("getHdf",
          signature(product = "character"),
          function(product, HdfName,
                   begin = NULL, end = NULL, 
                   tileH = NULL, tileV = NULL, extent = NULL, 
                   collection = NULL, wait = 0.5, 
                   checkIntegrity = TRUE, forceDownload = TRUE, ...) {
            
  ## if 'HdfName' is provided, call 'missing'-method          
  if (!missing(HdfName)) 
    getHdf(HdfName = HdfName, wait = wait, checkIntegrity = checkIntegrity, ...)
            
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
              isIn <- doCheckIntegrity(paste0(path$localPath,dates[[l]][i,-1]), wait=wait, opts = opts)
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
### function using 'missing' input #############################################
#' @aliases getHdf,missing-method
#' @rdname getHdf
setMethod("getHdf",
          signature(product = "missing"),
          function(HdfName, wait = 0.5, checkIntegrity = TRUE, ...) {
            
            opts <- combineOptions(...)
            
            ## if 'quiet' is not available, show full console output
            if (!"quiet" %in% names(opts))
              opts$quiet <- FALSE
            
            wait <- as.numeric(wait)
            
            ## loop over 'HdfName'
            if (inherits(HdfName, "list"))
              HdfName <- unlist(HdfName)
            
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
