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
#' @param begin,end \code{Date} or \code{character}. Begin and end date of MODIS 
#' time series, see \code{\link{transDate}}. 
#' @param tileH,tileV \code{numeric} or \code{character}. Horizontal and 
#' vertical tile number, see \code{\link{getTile}}.
#' @param extent See Details in \code{\link{getTile}}.
#' @param collection Desired MODIS product collection as \code{character}, 
#' \code{integer}, or \code{list} as returned by \code{\link{getCollection}}.
#' @param HdfName \code{character} vector or \code{list}. Full HDF file name(s) 
#' to download a small set of files. If specified, other file-related parameters 
#' (i.e., \code{begin}, \code{end}, \code{collection}, etc.) are ignored. 
#' @param checkIntegrity \code{logical}. If \code{TRUE} (default), the size of 
#' each downloaded file is checked. In case of inconsistencies, the function 
#' tries to re-download broken files. 
#' @param forceDownload \code{logical}. If \code{TRUE} (default), try to 
#' download data irrespective of whether online information could be retrieved 
#' via \code{MODIS:::getStruc} or not.
#' @param ... Further arguments passed to \code{\link{MODISoptions}}, eg 'wait'.
#' 
#' @return 
#' An invisible vector of downloaded data and paths.
#' 
#' @references 
#' MODIS data is currently available from the online data pools at 
#' \itemize{
#' \item{NASA Land Processes Distributed Active Archive Center (\href{https://lpdaac.usgs.gov/}{LP DAAC})},
#' \item{Level-1 and Atmosphere Archive & Distribution System (\href{https://ladsweb.modaps.eosdis.nasa.gov/}{LAADS})}, and
#' \item{National Snow & Ice Data Center (\href{https://nsidc.org/}{NSIDC})}.
#' }
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
#' # Get all MODIS Terra and Aqua M*D11A1 data from the past 30 days
#' # (can be run in a scheduled job for regular archive update)
#' b1 <- getHdf(product = "M.D13A2", begin = Sys.Date() - 30, 
#'              tileH = 18:19, tileV = 4) 
#' b1               
#' 
#' # Same tiles with a 'list' extent
#' Austria <- extent(9.2, 17.47, 46.12, 49.3)
#' b2 <- getHdf(product = "MOD13A2", begin = "2020001", end = "2020031", extent = Austria)
#' b2
#' 
#' # Using country boarders from 'mapdata' package
#' c <- getHdf(product = "MOD13A2", begin = "2016180", end = "2016210",
#'             extent = "Luxembourg")
#' c             
#'        
#' # Interactive selection of spatial extent, see getTile()
#' d <- getHdf(product = "MOD13A2", begin = "2016180", end = "2016210", extent = getTile())
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
                   collection = NULL, checkIntegrity = TRUE, 
                   forceDownload = TRUE, ...) {
            
  ## if 'HdfName' is provided, call 'missing'-method          
  if (!missing(HdfName)) 
    getHdf(HdfName = HdfName, checkIntegrity = checkIntegrity, ...)
            
  opts <- combineOptions(...)
            
  sturheit <- stubborn(level=opts$stubbornness)
  wait     <- as.numeric(opts$wait)
  
  # TODO HdfName as regex
  if (missing(product))
    stop("Please provide a supported 'product', see getProduct().\n")
  
  #######
  # check product
  product <- getProduct(x = product, quiet = TRUE
                        , collection = collection, forceCheck = TRUE)
  #########
  
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
  
  dates  <- list()
  output <- list() # path info for the invisible output
  l=0
  
  for(z in seq_along(product@PRODUCT))
  { # Platforms MOD/MYD
    
      todo <- paste0(product@PRODUCT[z],".",product@CCC[[z]])
      for (u in seq_along(todo))
      {
        # tileID
        if (product@TYPE[z]=="CMG") 
        {
          tileID="GLOBAL"
          ntiles=1 
        } else 
        {
          if (!inherits(extent, "MODISextent")) {
            extent = getTile(x = extent, tileH = tileH, tileV = tileV)
          }
          
          if (product@TYPE[z] == "Tile") {
            tileID <- extent@tile
            ntiles <- length(tileID)
          } else {
            tileID = NULL; ntiles = 0
          }
        }
        
        ## ensure compatibility with servers other than those specified in 
        ## `opts$MODISserverOrder`, e.g. when downloading 'MOD16A2' from NTSG
        server <- product@SOURCE[[z]]

        if (!any(server == "NSIDC")) {
          
          ## if product is not available from desired server, throw error        
          if (!any(opts$MODISserverOrder %in% server)) {
            stop(paste(product@PRODUCT[z], product@CCC[[z]], sep = ".")
                 , " is available from "
                 , paste(server, collapse = " and ")
                 , " only, please adjust 'MODISoptions(MODISserverOrder = ...)' accordingly.")
          }
          
          ## align with servers specified in 'MODISserverOrder' -> idenfify 
          ## priority and, if applicable, alternative download server
          server = unlist(sapply(opts$MODISserverOrder, function(i) {
            grep(i, server, value = TRUE)
          }))
          
        }
        
        server_alt = ifelse(length(server) > 1, server[2], NA)
        server = server[1]
        
        opts$MODISserverOrder = as.character(na.omit(c(server, server_alt)))

        ## this time, suppress console output from `getStruc`
        jnk <- capture.output(
          onlineInfo <- suppressWarnings(
            getStruc(product = product@PRODUCT[z], server = server, 
                     collection = product@CCC[[z]], begin = tLimits$begin, 
                     end = tLimits$end, wait = wait)
          )
        )
        
        if(!is.na(onlineInfo$online))
        {
          if (!onlineInfo$online & !is.na(server_alt) & 
              server %in% c("LPDAAC", "LAADS"))
          {
            cat(server," seems not online, trying on '",server_alt,"':\n",sep="")
            jnk = capture.output(
              onlineInfo <- getStruc(product = product@PRODUCT[z], collection = product@CCC[[z]],
                                     begin = tLimits$begin, end = tLimits$end, 
                                     wait = wait, server = server_alt)
            )
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
        
        st = correctStartDate(tLimits$begin, sel, product@PRODUCT[z]
                              , quiet = opts$quiet)
        us = sel >= st & sel <= tLimits$end

        if (sum(us,na.rm=TRUE)>0)
        { 
          suboutput <- list()
          l=l+1                
          dates[[l]] <- datedirs[us]
          
          dates[[l]] <- cbind(as.character(dates[[l]]),matrix(rep(NA, length(dates[[l]])*ntiles),ncol=ntiles,nrow=length(dates[[l]])))
          colnames(dates[[l]]) <- c("date",tileID)
          
          for (i in 1:nrow(dates[[l]]))
          { 
            year <- format(as.Date(dates[[l]][i,1]), "%Y")
            doy  <- as.integer(format(as.Date(dates[[l]][i,1]), "%j"))
            doy  <- sprintf("%03d",doy)
            mtr  <- rep(1,ntiles) # for file availability flaging
            path <- genString(x = strsplit(todo[u], "\\.")[[1]][1]
                              , collection = strsplit(todo[u], "\\.")[[1]][2]
                              , date = dates[[l]][i, 1])

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
                ftpfiles <- unlist(lapply(strsplit(ftpfiles," "), function(x) {
                  x[length(x)]
                })) # found empty dir!
                
                if (onlineInfo$source == "NTSG") {
                  ftpfiles = gsub(paste0("\\.", product@CCC[[z]], "\\.")
                                  , ifelse(product@PF3 == "MOD16", ".105.", ".305.")
                                  , ftpfiles)
                }
                
                for(j in 1:ntiles)
                { # j=1
                  if(mtr[j]==1)
                  { # if tile is missing get it
                    dts = dates[[l]][i, j+1]
                    if (onlineInfo$source == "NTSG") {
                      dts = gsub(paste0("\\.", product@CCC[[z]], "\\.")
                                 , ifelse(product@PF3 == "MOD16", ".105.", ".305.")
                                 , dts)
                      dts = paste(c(strsplit(dts, "\\.")[[1]][1:4], "*.hdf"), collapse = ".")
                    }
                    onFtp = grep(ftpfiles,pattern = dts,value = TRUE)
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
                      hdf <- do.call(ModisFileDownloader, c(list(x = HDF), opts))
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
            
            if(checkIntegrity & !all(is.na(dates[[l]][i, -1])))
            { # after each 'i' do the sizeCheck
              isIn <- do.call(doCheckIntegrity
                              , c(list(x = paste0(path$localPath
                                                  , na.omit(dates[[l]][i,-1])))
                                  , opts))
            }
            suboutput[[i]] <- ifelse(is.na(dates[[l]][i,-1]), NA, paste0(path$localPath,dates[[l]][i,-1]))
          } # end i
          
          output[[l]] <-  as.character(na.omit(unlist(suboutput)))
          # if (length(output[[l]]) == 0) output[[l]] = NA
          names(output)[l] <- todo[u]
        } else 
        {
          warning(paste("No", product@PRODUCT, "files found for the period from"
                        , tLimits$begin, "to", paste0(tLimits$end, ".")))
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
          function(HdfName, checkIntegrity = TRUE, ...) {
            
            opts <- combineOptions(...)
            
            wait <- as.numeric(opts$wait)
            
            ## loop over 'HdfName'
            if (inherits(HdfName, "list"))
              HdfName <- unlist(HdfName)
            
            HdfName <- basename(HdfName)
            
            dates <- sapply(HdfName, function(i) {

              # if missing, add .hdf file extension
              if (!grepl(".hdf$", i, ignore.case = TRUE)) {
                i = paste0(i, ".hdf")
              }              
              
              path <- do.call(genString, c(list(x = i), opts))
              path$localPath <- setPath(path$localPath)
              
              if (!file.exists(paste0(path$localPath, "/", i))) 
                do.call(ModisFileDownloader, c(list(x = i), opts))
              
              if(checkIntegrity)
                jnk <- do.call(doCheckIntegrity, c(list(x = i), opts))
              
              gsub("//", "/", paste0(path$local, "/", i))
            })
            
            ## return output
            return(invisible(dates))
          })
