# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date: August 2011
# Licence GPL v3

# product="MOD13Q1"; collection=NULL; server="LPDAAC"; begin=NULL; end=NULL; forceCheck=FALSE; wait=0; stubbornness=1

getStruc <- function(product, collection=NULL, server=getOption("MODIS_MODISserverOrder")[1], begin=NULL, end=NULL, forceCheck=FALSE, wait=1, stubbornness=10)
{
  server <- toupper(server)[1]
  if(!server %in% c("LPDAAC", "LAADS", "NTSG"))
  {
    stop("getStruc() Error! Server must be one of 'LPDAAC', 'LAADS' or 'NTSG'.")
  }
  opts     <- combineOptions()
  sturheit <- stubborn(level=stubbornness)
  
  setPath(opts$auxPath, ask=FALSE)
  #########################
  # Check Platform and product
  product <- getProduct(x=product,quiet=TRUE)
  # Check collection
  if (!is.null(collection))
  {
    product$CCC <- getCollection(product=product,collection=collection) 
  }
  if (length(product$CCC)==0)
  {
    product$CCC <- getCollection(product=product) # if collection isn't provided, this gets the newest for the selected products.
  }
  
  dates <- transDate(begin=begin,end=end)
  todoy <- format(as.Date(format(Sys.time(),"%Y-%m-%d")),"%Y%j")
  ########################
  
  # load aux
  col    <- product$CCC[[1]]
  basnam <- paste0(product$PRODUCT[1],".",product$CCC[[1]],".",server)
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
    
    path <- genString(x=product$PRODUCT[1], collection=col, local=FALSE)
    
    cat("Downloading structure on '",server,"' for: ",product$PRODUCT[1],".",col,"\n",sep="")
    
    if(exists("FtpDayDirs"))
    {
      rm(FtpDayDirs)
    }
    
    if (server=="LPDAAC")
    {
      startPath <- strsplit(path$remotePath$LPDAAC,"DATE")[[1]][1] # cut away everything behind DATE
      for (g in 1:sturheit)
      {
        cat("Try:",g," \r")
        FtpDayDirs <- try(filesUrl(startPath))
        cat("             \r")
        if(exists("FtpDayDirs"))
        {    
          break
        }
        Sys.sleep(wait)
      }
      FtpDayDirs <- as.Date(as.character(FtpDayDirs),"%Y.%m.%d")
    } else if (server=="LAADS")
    {
      startPath <- strsplit(path$remotePath$LAADS,"YYYY")[[1]][1] # cut away everything behind YYYY
      opt <- options("warn")
      options("warn"=-1)
      rm(years)
      
      once <- TRUE
      for (g in 1:sturheit)
      {
        cat("Downloading structure from 'LAADS'-server! Try:",g,"\r")
        years <- try(filesUrl(startPath))
        years <- as.character(na.omit(as.numeric(years))) # removes folders/files probably not containing data
        
        if(g < (sturheit/2))
        {
          Sys.sleep(wait)
        } else
        {
          if(once & (30 > wait)) {cat("Server problems, trying with 'wait=",max(30,wait),"\n")}
          once <- FALSE                        
          Sys.sleep(max(30,wait))
        }
        if(exists("years"))
        {    
          break
        }
        cat("                                                      \r") 
      }
      options("warn"=opt$warn)
      
      Ypath <- paste0(startPath,years,"/")
      
      ouou <- vector(length=length(years),mode="list")
      for(ix in seq_along(Ypath))
      {
        cat("Downloading structure of '",years[ix],"' from '",server,"'-server.                        \r",sep="")
        ouou[[ix]] <- paste0(years[ix], filesUrl(Ypath[ix]))
      }
      cat("                                                                    \r")
      FtpDayDirs <- as.Date(unlist(ouou),"%Y%j")
      
    } else if (server == "NTSG") {
      
      startPath <- strsplit(path$remotePath$NTSG,"YYYY")[[1]][1] # cut away everything behind YYYY
      opt <- options("warn")
      options("warn"=-1)
      rm(years)
      
      once <- TRUE
      for (g in 1:sturheit)
      {
        cat("Downloading structure from 'NTSG'-server! Try:", g, "\n")
        years <- try(filesUrl(startPath))
        
        ## remove 'Geotiff' folder; applies for 'MOD16A3' only
        if (length(grep("Geotiff", years) > 0))
          years <- years[-grep("Geotiff", years)]
        
        years_new <- gsub("^Y", "", years)

        if(g < (sturheit/2))
        {
          Sys.sleep(wait)
        } else
        {
          if(once & (30 > wait)) {cat("Server problems, trying with 'wait=",max(30,wait),"\n")}
          once <- FALSE                        
          Sys.sleep(max(30,wait))
        }
        if(exists("years"))
        {    
          break
        }
        cat("                                                      \r") 
      }
      options("warn"=opt$warn)
      
      if (product$PRODUCT != "MOD16A3") {
        Ypath <- paste0(startPath,years,"/")
        
        ouou <- vector(length=length(years),mode="list")
        cat("Downloading structure from", years_new[1], "to", years_new[length(years_new)], "from", server, "file server.\n")
        for(ix in seq_along(Ypath))
        {
          ouou[[ix]] <- paste(years[ix], filesUrl(Ypath[ix]), sep = "/")
        }
        cat("                                                                    \r")
        FtpDayDirs <- as.Date(unlist(ouou),"Y%Y/D%j")
      
      ## if product is 'MOD16A3', no daily sub-folders exist    
      } else {
        FtpDayDirs <- as.Date(paste(years_new, "12", "31", sep = "-"))
      }
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
