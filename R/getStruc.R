# Author: Matteo Mattiuzzi, Florian Detsch
# Date: January 2018
getStruc <- function(product, collection = NULL, server = NULL, begin = NULL
                     , end = NULL, forceCheck = FALSE, ...)
{

  opts     <- combineOptions(...)
  sturheit <- stubborn(level = opts$stubbornness)

  setPath(opts$auxPath, ask=FALSE)
  
  #########################
  # Check product 
  if (!inherits(product, "MODISproduct")) {
    inp = product
    product <- getProduct(x = product, quiet = TRUE
                          , collection = collection, forceCheck = forceCheck)
    
    if (is.null(product)) {
      stop("Product '", inp, "' not recognized. See getProduct() for a list of "
           , "available products.")
    } else rm(inp)
    
    # Check collection
    product@CCC = getCollection(product = product, collection = collection
                                , forceCheck = forceCheck) 
  }
  
  # Check server
  if (is.null(server)) {
    server = unlist(product@SOURCE)[1]
    
  } else if (!server %in% (srv <- unlist(product@SOURCE))) {
    if (!opts$quiet) {
      warning(paste0(product@PRODUCT, ".", product@CCC), " is not available from "
              , server, ", retrieving structure from ", srv[1], " instead.")
    }
    
    server = srv[1]
  }
  
  dates <- transDate(begin=begin,end=end)
  todoy <- format(as.Date(format(Sys.time(),"%Y-%m-%d")),"%Y%j")
  ########################
  
  # load aux
  col    <- product@CCC[[1]]
  basnam <- paste0(product@PRODUCT[1],".",product@CCC[[1]],".",server)
  info   <- list.files(path=opts$auxPath,pattern=paste0(basnam,".*.txt"),full.names=TRUE)[1]
  
  output <- list(dates=NULL,source=server,online=NA)
  class(output) <- "MODISonlineFolderInfo" 
  
  ## no local structure
  if (is.na(info)) {
    getIT <- TRUE
    online <- TRUE
    
  ## if local structure exists, check if  
  } else {
    lastcheck    <- as.Date(strsplit(basename(info),"\\.")[[1]][4],"%Y%j")
    output$dates <- na.omit(as.Date(read.table(info,stringsAsFactors=FALSE)[,1]))
    
    # end date in local structure is younger than user-defined end date
    if (max(output$dates,na.rm=TRUE) > dates$end) {
      getIT <- FALSE
      online <- "up-to-date"
      
    } else {
      
      # last check is older than 24 hours  
      if (lastcheck < as.Date(todoy,"%Y%j")) {
        getIT <- TRUE
        online <- TRUE
        
      # last check is not older than 24 hours, but end date in local structure 
      # is older than user-defined end date
      } else {
        getIT <- FALSE
        online <- "up-to-date"
      }
    }
  }
  
  if (getIT | forceCheck)
  {

    lockfile <- paste0(opts$auxPath, basnam,".lock")[[1]]
    if(file.exists(lockfile))
    {
      if(as.numeric(Sys.time() - file.info(lockfile)$mtime) > 10)
      {
        unlink(lockfile)
      } else
      {
        readonly <- TRUE
      }
    } else
    {
      zz <- file(description=lockfile, open="wt")  # open an output file connection
      write('deleteme',zz)
      close(zz)
      
      readonly <- FALSE
      on.exit(unlink(lockfile))
    }
    
    path <- genString(x=product@PRODUCT[1], collection=col, local=FALSE)
    
    cat("Downloading structure on '",server,"' for: ",product@PRODUCT[1],".",col,"\n",sep="")
    
    if(exists("FtpDayDirs"))
    {
      rm(FtpDayDirs)
    }
    
    servers = intersect(
      unlist(product@SOURCE)
      , server
    )
    
    FtpDayDirs = try(log("e"), silent = TRUE)
    g = 1L
    
    while (inherits(FtpDayDirs, "try-error") & g <= sturheit) {
      server = ifelse(
        length(servers) > 1
        , servers[(g %% length(servers) == 0) + 1L]
        , servers
      )
      startPath = unlist(strsplit(path$remotePath[[server]], "DATE|YYYY"))[1] # cut away everything behind DATE
      
      if (g > 1L) Sys.sleep(opts$wait)
      FtpDayDirs = try(filesUrl(startPath), silent = TRUE)
      g = g + 1L
    }
    
    
    ### . lpdaac, nsidc ----
    
    FtpDayDirs = if (server %in% c("LPDAAC", "NSIDC")) {
      
      na.omit(as.Date(as.character(FtpDayDirs),"%Y.%m.%d"))
      
      ### . laads ----
      
    } else if (server=="LAADS") {
      
      Ypath = paste0(startPath, FtpDayDirs, "/")
      
      ouou = mapply(function(i, j) {
        cat("\t...", i, "\n")
        ou = try(log("e"), silent = TRUE)
        g = 1L
        
        while (inherits(ou, "try-error") & g <= sturheit) {
          if (g > 1) Sys.sleep(opts$wait)
          ou = try(paste0(i, filesUrl(j)), silent = TRUE)
          g = g + 1L
        }
        
        return(ou)
      }, FtpDayDirs, Ypath, SIMPLIFY = FALSE, USE.NAMES = FALSE)
      as.Date(unlist(ouou),"%Y%j")
    } 
  }
  
  if(!exists("FtpDayDirs"))
  {

    if (online == "up-to-date") {
      cat("Local structure is up-to-date. Using offline information!\n")
      output$online <- TRUE
    } else {
      cat("Couldn't get structure from", server, "server. Using offline information!\n")
      output$online <- FALSE
    }
    
  } else if (FtpDayDirs[1]==FALSE)
  {
    cat("Couldn't get structure from", server, "server. Using offline information!\n")
    output$online <- FALSE
  } else
  {
    output$dates  <- FtpDayDirs
    output$online <- TRUE
  }
  
  if (exists("readonly")) {
    if(!readonly)
    {
      unlink(list.files(path=opts$auxPath, pattern=paste0(basnam,".*.txt"), full.names=TRUE))
      unlink(lockfile)
      write.table(output$dates, paste0(opts$auxPath,basnam,".",todoy,".txt"), row.names=FALSE, col.names=FALSE)  
    }
  }
  
  return(output)
}  
