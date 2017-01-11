#' Set or Retrieve Permanent MODIS Package Options
#' 
#' @description 
#' Set or retrieve persistant \strong{MODIS} package options (per user or 
#' systemwide). Changes here will persist through sessions and updates.
#' 
#' @param localArcPath \code{character}, defaults to \code{"~/MODIS_ARC"}. 
#' Target folder for downloaded MODIS HDF files. 
#' @param outDirPath \code{character}, defaults to \code{"~/MODIS_ARC/PROCESSED"}. 
#' Target folder for results of \code{\link{runGdal}} and \code{\link{runMrt}}. 
#' @param pixelSize Output pixel size (in target reference system units) passed 
#' to \code{\link{runGdal}} and \code{\link{runMrt}}, defaults to \code{"asIn"}.
#' @param outProj Target reference system passed to \code{\link{runGdal}} and 
#' \code{\link{runMrt}}. \code{\link{runGdal}} requires a valid 
#' \code{\link{CRS}}. As for \code{\link{runMrt}}, please consult the MRT manual. 
#' Since the two pocessing methods do not have common methods, it is suggested 
#' to stick with the default settings (see Details).
#' @param resamplingType Defaults to \code{"NN"} (Nearest Neightbour). MRT and 
#' GDAL both support \code{c('NN', 'CC', 'BIL')}. In addition, GDAL supports 
#' \code{cubicspline} and \code{lanczos} and, from \code{GDAL >= 1.10.0} onwards, 
#' also \code{mode} and \code{average}.
#' @param dataFormat \code{character}, defaults to \code{"GTiff"}. One of 
#' \code{getOption("MODIS_gdalOutDriver")} (column 'name').
#' @param gdalPath \code{character}. Path to gdal bin directory and more 
#' relevant for Windows users. Use \code{MODIS:::checkTools("GDAL")} to try to 
#' detect it automatically.
#' @param MODISserverOrder \code{character}. Possible options are \code{"LAADS"}
#' (default) and \code{"LPDAAC"} (see 'dlmethod' and 'Details'). If only one 
#' server is selected, all efforts to download data from the second server 
#' available are inhibited.
#' @param dlmethod \code{character}, defaults to \code{auto}. See 'method' in 
#' \code{\link{download.file}}. On Unix (also Mac?), it is suggested to use 
#' \code{"wget"} or, if installed, \code{"aria2"}. In order to download MODIS 
#' files from LPDAAC, please note that either wget (default) or curl must be 
#' installed and made available through the PATH environmental variable.
#' @param stubbornness \code{numeric}. The number of retries after the target 
#' server has refused a connection. Higher values increase the chance of getting 
#' the file, but also lead to hanging functions if the server is down.
#' @param systemwide \code{logical}. If \code{FALSE} (default), 'user'-wide 
#' settings are saved to \code{path.expand("~/.MODIS_Opts.R")}. If \code{TRUE}, 
#' write settings to 'systemwide', presumed you have write access to 
#' \code{paste(R.home(component="etc"), '/', '.MODIS_opts.R', sep='')}.
#' @param quiet \code{logical}. If \code{FALSE} (default), options are printed 
#' to the console.
#' @param save \code{logical}. If \code{TRUE} (default), settings are permanent.
#' @param checkTools \code{logical}, defaults to \code{TRUE}. Check if external 
#' tools (i.e., GDAL and MRT) are installed and reachable through R.
#' 
#' @details 
#' These settings are permanent, easy to change and take effect immediately!
#' 
#' If you change default values, consider that your settings have to be valid 
#' for any MODIS product, layer and area!
#' 
#' It is not recommended to use 
#' \itemize{
#' \item{a coordinate reference system that is not applicable globally as 
#' default for 'outProj',}
#' \item{or a fixed 'pixelSize' for different products,}
#' \item{or a 'resamplingType' that is not \code{"NN"}.}
#' }
#' 
#' 'localArcPath' and 'outDirPath' should be changed, expecially on a Windows 
#' OS, as '~/MODIS_ARC/...' is normally on the 'C:/...' drive. You may also 
#' specify a shared network drive if you have a central MODIS data server. 
#' 
#' On Windows, you have to set 'gdalPath' to the location of GDAL executables 
#' (i.e., the '.../GDAL../bin' directory). On Unix-alikes, this should not be 
#' required unless you want to specify a non-default GDAL installation.
#' 
#' On an unixoid OS, it is suggested to use \code{dlmethod = 'wget'} because it 
#' is a reliable tool and, after the change of the 'LP DAAC' datapool from FTP 
#' to HTTP (May 2013), \code{dlmethod = 'auto'} seems not to work properly. On 
#' Windows, on the other hand, \code{dlmethod = 'auto'} seems to work fine. 
#' 
#' Please note that in order to download MODIS files from LPDAAC, you are 
#' required to register for an Earthdata Login Profile 
#' (\url{https://urs.earthdata.nasa.gov/users/new}) and create a read-only 
#' .netrc file in your home directory containing the Earthdata server address as 
#' well as your login credentials. An automated solution for the creation of a 
#' workable .netrc file is provided through \code{\link{lpdaacLogin}}. 
#' 
#' @author 
#' Matteo Mattiuzzi and Steven Mosher
#' 
#' @examples 
#' \dontrun{
#' ## get options
#' MODISoptions()
#' 
#' ## set options
#' MODISoptions(localArcPath="/another/path/than/default")
#' }
#' 
#' @export MODISoptions
#' @name MODISoptions
MODISoptions <- function(localArcPath, outDirPath, pixelSize, outProj, 
                         resamplingType, dataFormat, gdalPath, MODISserverOrder, 
                         dlmethod, stubbornness, systemwide = FALSE, 
                         quiet=FALSE, save=TRUE, checkTools = TRUE)
{
  # This function collects the package options from up to 3 files and creates the .MODIS_opts.R file (location depending on systemwide=T/F, see below):
  # 1. package installation directory (factory defaults); 
  # 2. /R/etc/.MODIS_opts.R for system wide settings (all users of a machine) and 
  # 3. user home "~/.MODIS_opts.R", for user specific settings. 
  # settings are collected in direction 1-3 and each time overwritten if available
  # The final settings are written in to the user specific file 3.
  # options are not tested here! only generated!
  
  # debug: systemwide = FALSE; quiet=FALSE; save=TRUE

  # container for all options
  opts  <- new.env()
  
  ##################################
  # 1. factory defaults -----
  eval(parse(file.path(find.package("MODIS"), "external", "MODIS_Opts.R")),
       envir = opts) 
  
  # 2. system wide -----
  sysopts <- paste(R.home(component = "etc"), '.MODIS_Opts.R', sep = '/')
  so      <- FALSE
  
  if (file.exists(sysopts)) {
    eval(parse(sysopts), envir = opts)
    so <- TRUE
  }
  
  # 3. user specific -----
  optfile <- file.path("~/.MODIS_Opts.R", fsep = "/")
  uo      <- FALSE
  
  # system-wide
  if(systemwide) {
    if(!file.create(sysopts, showWarnings = FALSE))
      stop("You do not have sufficient permissions to create or change ", 
           "'systemwide' MODIS options. Set 'systemwide = FALSE' for user-wide", 
           " settings or start R as root/admin and re-run MODISoptions().\n")

    optfile <- sysopts
    whose   <- 'systemwide'
    
  # user-wide  
  } else {
    if (file.exists(optfile)) {   
      eval(parse(optfile), envir = opts)
      uo <- TRUE
    }
    
    whose <- 'user'
  } 
  
  if(!uo) {
    if(!so & save) {
      warning("No MODIS 'user' nor 'systemwide' settings file found. File is created for '",whose,"'-settings in: ",normalizePath(optfile,'/',mustWork=FALSE),sep="")
    } else if (!save) {
      warning("No MODIS 'user' nor 'systemwide' settings file found, using factory defaults. Use '?MODISoptions' to configure the 'MODIS' package and make settings permanent!")
    }
  }
  
  #################################
  opt <- as.list(opts)	
  
  # localArcPath
  opt$localArcPath <- correctPath(opt$localArcPath)
  
  if(!missing(localArcPath)) {
    localArcPath <- correctPath(localArcPath) 
  
    if (opt$localArcPath != localArcPath) {
      message("Setting 'localArcPath' to '", normalizePath(localArcPath,"/",FALSE),"'\nIf you already have downloaded some HDF-files to '",normalizePath(opt$localArcPath,"/",FALSE) ,"' you can use '?orgStruc()' to re-arrange your HDF-data!")
    }
    
    options(MODIS_localArcPathWarned=TRUE)
    opt$localArcPath <- localArcPath
  } else {
    if (length(list.dirs(opt$localArcPath,recursive=FALSE))==0)
    {
      if(!isTRUE(options()$MODIS_localArcPathWarned))
      {
        message("'localArcPath' does not exist, and will be created in '",normalizePath(opt$localArcPath,"/",FALSE),"'. Consult '?MODISoptions' if you want to change it!")               
        options(MODIS_localArcPathWarned=TRUE)
      } 
    }
  }
  
  # outDirPath
  opt$outDirPath <- correctPath(opt$outDirPath)
  
  if(!missing(outDirPath))
  {
    outDirPath <- correctPath(outDirPath)
    
    if (length(list.dirs(opt$outDirPath,recursive=FALSE))==0)
    {
      message("'outDirPath' does not exist and will be created in '",normalizePath(outDirPath,"/",FALSE),"'")               
    } else if (opt$outDirPath != outDirPath)
    {
      message("'outDirPath' has been changed from '",normalizePath(opt$outDirPath,"/",FALSE),"' to '",normalizePath(outDirPath,"/",FALSE),"'")
    }
    options(MODIS_outDirPathWarned=TRUE) 
    opt$outDirPath <- outDirPath
  } else
  {
    if (length(list.dirs(opt$outDirPath,recursive=FALSE))==0)
    {
      if(!isTRUE(options()$MODIS_outDirPathWarned))
      {
        message("'outDirPath' does not exist, it will be created in '",normalizePath(opt$outDirPath,"/",FALSE),"'. Consult '?MODISoptions' if you want to change it!")               
        options(MODIS_outDirPathWarned=TRUE)
      }
    }    
  }

  opt$auxPath <- paste0(opt$outDirPath,".auxiliaries/")
  
  if(!missing(dlmethod))
  {
    dlmethod <- tolower(dlmethod)
    stopifnot(dlmethod %in% c("auto","internal","wget","curl","lynx","aria2"))
    opt$dlmethod <- dlmethod
  }
  
  if(!missing(stubbornness))
  {
    opt$stubbornness <- stubbornness
  }
  
  if(!missing(resamplingType))
  {
    stopifnot(tolower(resamplingType) %in% c('nn', 'cc', 'bil','near', 'bilinear', 'cubic','cubicspline','lanczos', 'average', 'mode'))
    opt$resamplingType <- resamplingType
  }
  
  if(!missing(outProj))
  {
    opt$outProj <- outProj
  }
  
  if(!missing(pixelSize))
  {
    opt$pixelSize <- pixelSize
  }
  
  if (!missing(gdalPath))
  {
    opt$gdalPath <- correctPath(gdalPath)
    if(length(grep(dir(opt$gdalPath),pattern="gdalinfo"))==0)
    {
      stop(paste0("The 'gdalPath' you have provided '",normalizePath(opt$gdalPath,"/",FALSE) ,"' does not contain any gdal utilities, make sure to address the folder with GDAL executables (ie: gdalinfo)!"))
    }
  }
  opt$gdalPath <- correctPath(opt$gdalPath)
  options(MODIS_gdalPath=opt$gdalPath) # needs to be exportet now as it is required by checkTools a few lines bellow (uses combineOptions())! Maybe not the best solution!
  
  if(is.null(opt$MODISserverOrder))
  {
    opt$MODISserverOrder <- c("LAADS", "LPDAAC")
  }
  if (!missing(MODISserverOrder))
  {
    MODISserverOrder <- toupper(MODISserverOrder)
    if(length(MODISserverOrder)==1)
    {
      if("LPDAAC" %in% MODISserverOrder | "LAADS" %in% MODISserverOrder)
      {
        opt$MODISserverOrder <- MODISserverOrder   
      }
    } else if(length(MODISserverOrder)==2)
    {
      if("LPDAAC" %in% MODISserverOrder & "LAADS" %in% MODISserverOrder)
      {
        opt$MODISserverOrder <- MODISserverOrder   
      }
    } else
    {
      stop("Provide valid 'MODISserverOrder' see '?MODISoptions'") 
    }
  }  
  
  # checks if the pointed GDAL exists and supports 'HDF4Image' driver.
  if(checkTools)
  {
    # GDAL
    isOk <- checkGdalDriver(path=opt$gdalPath)
    if (isOk) 
    {
      opt$gdalOk  <- TRUE
      gdalVersion <- checkTools(tool="GDAL",quiet=TRUE, opts = opt)$GDAL$version
    } else
    {
      opt$gdalOk  <- FALSE
      gdalVersion <- "Not available. Use 'MODIS:::checkTools('GDAL')' for more information!"
    }
    
    # MRT
    mrt <- checkTools(tool="MRT",quiet=TRUE, opts = opt)$MRT
    if(mrt$MRT)
    {
      opt$mrtOk  <- TRUE
      mrtVersion <- mrt$version
    } else
    {
      opt$mrtOk  <- FALSE
      mrtVersion <- "Not available. Use 'MODIS:::checkTools('MRT')' for more information!"
    }
  } else
  {
    if(!isTRUE(opt$gdalOk)) # if TRUE, MODIS has all info it requires about GDAL
    {
      opt$gdalOk <- FALSE
    }
    if(!isTRUE(opt$mrtOk)) # if TRUE, MODIS has all info it requires about MRT
    {
      opt$mrtOk <- FALSE
    }
    gdalVersion <- "Not checked, run 'MODISoptions(checkPackages=TRUE)'"
    mrtVersion  <- "Not checked, run 'MODISoptions(checkPackages=TRUE)'"
  }   
  
  if(!missing(dataFormat))
  {
    opt$dataFormat <- dataFormat
  }
  if(is.null(opt$dataFormat))
  {
    opt$dataFormat <- 'GTiff'
  }
  
  if(checkTools & opt$gdalOk)
  {
    opt$gdalOutDriver <- gdalWriteDriver(renew = FALSE, quiet = FALSE, gdalPath=opt$gdalPath,outDirPath=opt$outDirPath)
  }
  
  if (save)
  {    
    #  create the '.MODIS_opts.R' file
    filename <- file(optfile, open="wt")
    
    write(paste('# This file contains ', whose,' default values for the R package \'MODIS\'.',sep=""), filename)
    write('# version 0.9-14', filename)
    write('# consult \'?MODISoptions\' for details and explanations', filename)
    write('  ', filename)
    write('#########################', filename)
    write('# 1.) Path and archive structure defaults.', filename)
    write('# consult \'?MODISoptions\' for more details', filename)
    write('# USE SINGLE FORWARD SLASH "/" (also on WINDOWS)', filename)
    write('# If path does not exist it is created!', filename)
    write('# Work also with network share!', filename)
    write('  ', filename)
    
    write('# All HDF-data will be (properly) stored in this directory.',filename)	
    write(paste0('localArcPath <- \'',normalizePath(opt$localArcPath,"/",FALSE),'\''), filename)    
    write('  ', filename)
    
    write('# Default output location for MODIS package processing results.',filename)
    write(paste0('outDirPath   <- \'',normalizePath(opt$outDirPath,"/",FALSE), '\''),filename)
    write('  ', filename)
    
    write('#########################', filename)
    write('# 2.) download defaults:', filename)
    write('# consult \'?MODISoptions\' for more details', filename)
    write('  ', filename)
    
    if(length(opt$MODISserverOrder)==2)
    {
      write(paste0('MODISserverOrder <- c(\'',paste(opt$MODISserverOrder,collapse="', '"),'\')' ), filename)
    } else
    {
      write(paste0('MODISserverOrder <- \'',opt$MODISserverOrder,'\'' ), filename)
    }
    write(paste0('dlmethod         <- \'',opt$dlmethod,'\'' ), filename)
    write(paste0('stubbornness     <- \'',opt$stubbornness,'\''), filename)
    write('  ', filename)
    
    write('#########################', filename)
    write('# 3.) Processing defaults:', filename)
    write('# It is highly recommended to not modify here, at least not \'resamplingType\' as there are several layers that require NN (i.e. \'VI_Quality\', \'Day of the year\',...)!', filename)
    write('# consult \'?MODISoptions\' for more details', filename)
    write('  ', filename)
    write(paste0('resamplingType <- \'',opt$resamplingType,'\''), filename)
    write(paste0('outProj        <- \'',opt$outProj,'\''),filename)
    write(paste0('pixelSize      <- \'',opt$pixelSize,'\''),filename)
    write(paste0('dataFormat     <- \'',opt$dataFormat,'\''),filename)
    write('  ', filename)	
    write('#########################', filename)
    write('# 4.) Set path to GDAL _bin_ directory', filename)
    write('# More related to Windows, but also to other OS in case of a non standard location of GDAL', filename)
    write('# ON WINDOWS install \'OSGeo4W\' (recommanded) or \'FWTools\'', filename)
    write('# consult \'?MODISoptions\' for more details', filename)        
    write('# Run: \'MODIS:::checkTools()\' to try to autodetect location.', filename)
    write('# Example (USE SINGLE FORWARD SLASH \'/\'!):', filename)
    write('# gdalPath <- \'C:/OSGeo4W/bin/\'', filename)
    write('  ', filename)
    if (!is.null(opt$gdalPath))
    {
      write(paste0("gdalPath <- '",normalizePath(opt$gdalPath,"/",FALSE),"'"), filename)
    }
    
    write('  ', filename)	
    write('#########################', filename)
    write('#########################', filename)        
    write('# 5.) Package internal information, do ot change manually', filename)
    write('  ', filename)	
    write(paste0('gdalOk <- ', opt$gdalOk), filename)	
    write(paste0('mrtOk  <- ', opt$mrtOk), filename)	        
    write('  ', filename)	
    close(filename)
  }
  
  if (!quiet) 
  {
    cat('\nSTORAGE:\n')
    cat('_______________\n')
    cat('localArcPath :', normalizePath(opt$localArcPath,"/",FALSE), '\n' )
    cat('outDirPath   :', normalizePath(opt$outDirPath,"/",FALSE), '\n\n\n')
    
    cat('DOWNLOAD:\n')
    cat('_______________\n')
    cat('MODISserverOrder :', paste(opt$MODISserverOrder,collapse=", "),'\n')
    cat('dlmethod         :', opt$dlmethod,'\n')
    cat('stubbornness     :', opt$stubbornness,'\n\n\n')
    
    cat('PROCESSING:\n')
    cat('_______________\n')
    cat('GDAL           :', gdalVersion, '\n')
    cat('MRT            :', mrtVersion, '\n')
    cat('pixelSize      :', opt$pixelSize, '\n')
    cat('outProj        :', opt$outProj, '\n')
    cat('resamplingType :', opt$resamplingType, '\n')
    cat('dataFormat     :', opt$dataFormat, '\n\n\n')
  }
  
  # remove ftpstring* from opt (old "~/.MODIS_Opts.R" style)
  oldftp <- grep(names(opt),pattern="^ftpstring*")
  
  if(length(oldftp)>1)
  {
    opt <- opt[-oldftp]
  }
  
  # set the options
  for (i in seq_along(opt))
  {
    if (is.character(opt[[i]]))
    {
#      if(length(opt[[i]])==1)
#      {
#        eval(parse(text=paste0("options(MODIS_",names(opt[i]),"='",opt[[i]],"')")))
#      } else
#      {
        eval(parse(text=paste0("options(MODIS_",names(opt[i]),"= c('",paste0(opt[[i]],collapse="', '"),"'))")))
#      }
    } else if (is.data.frame(opt[[i]]) | is.matrix(opt[[i]]))
    {
      eval(parse(text=paste0("options(MODIS_",names(opt[i]),"=opt$",names(opt[i]),")")))
    } else
    {
      eval(parse(text=paste0("options(MODIS_",names(opt[i]),"=",opt[[i]],")")))        
    }
  }
  # this is fixed
  options(MODIS_arcStructure='/SENSOR/PRODUCT.CCC/DATE')
  return(invisible())
}


