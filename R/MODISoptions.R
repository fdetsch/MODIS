#' Set or Retrieve Permanent MODIS Package Options
#' 
#' @description 
#' Set or retrieve **MODIS** package options (per user or system-wide). If 
#' `save = TRUE` (default), changes will persist through sessions and updates.
#' 
#' @param localArcPath `character`, defaults to 
#'   `file.path(tempdir(), "MODIS_ARC")`. Target folder for downloaded MODIS HDF
#'   files. 
#' @param outDirPath `character`, defaults to 
#'   `file.path(tempdir(), "MODIS_ARC/PROCESSED")`. Target folder for results 
#'   of [runGdal()] and [runMrt()]. 
#' @param pixelSize Output pixel size (in target reference system units) passed 
#'   to [runGdal()] and [runMrt()], defaults to `"asIn"`.
#' @param outProj Target reference system passed to [runGdal()] and 
#'   [runMrt()]. [runGdal()] requires a valid CRS. As for [runMrt()], please 
#'   consult the MRT manual. Since the two processing methods do not have common
#'   methods, it is suggested to stick with the default settings (see Details).
#' @param resamplingType Defaults to `"NN"` (Nearest Neighbor). MRT and GDAL 
#'   both support `c('NN', 'CC', 'BIL')`. GDAL additionally supports all 
#'   resampling methods listed under <https://gdal.org/programs/gdalwarp.html>.
#' @param dataFormat `character`, defaults to `"GTiff"`. See
#'   `sf::st_drivers(what = "raster")` for available options.
#' @param gdalPath `character`. Path to GDAL binary executable, used to relate 
#'   writable `sf::st_drivers("raster")` to file extensions in case of 
#'   non-standard formats.
#' @param MODISserverOrder `character`. Possible options are `"LPDAAC"` 
#'   (default), `"LAADS"` (see 'dlmethod' and Details). If only one server is 
#'   selected, all efforts to download data from the second server are 
#'   inhibited.
#' @param dlmethod `character`, defaults to `"auto"`. See 'method' in 
#'   [utils::download.file()] for available options. If installed, you can also 
#'   leverage `"aria2"` for multi-source download. Note that in any case, either
#'   curl or wget must be installed and made available through the system's PATH
#'   environment variable to validate Earthdata Login credentials.
#' @param stubbornness `numeric`. The number of retries after the target server 
#'   has refused a connection. Higher values increase the chance of getting the 
#'   file, but also lead to hanging functions if the server is down.
#' @param wait `numeric` waiting time (in seconds) inserted after each internal 
#'   online download call via [utils::download.file()] or [curl::curl()]. 
#'   Reduces the chance of connection errors that may occur after many requests.
#' @param cellchunk Default `1`, i.e. use raster default. Comparable with 
#'   'chunksize' in [raster::rasterOptions()], but as no effect was found in 
#'   adapting chunksize, **MODIS** applies its own variant. On a reasonable 
#'   working station, 'cellchunk' can easily be increased to `5e5`.
#' @param systemwide A `logical` determining whether changes made to 
#'   [MODISoptions()] are to be applied system-wide or per user (default), see 
#'   Details.
#' @param quiet `logical` passed e.g. to [utils::download.file()] and
#'   [sf::gdal_utils()]. 
#' @param save `logical`. If `TRUE` (default), settings are permanent.
#' @param checkTools `logical`, defaults to `TRUE`. Check if external tools 
#'   (i.e., GDAL and MRT) are installed and reachable through R.
#' @param checkWriteDrivers `logical`. If `TRUE` (default), find write drivers 
#'   supported by **sf** GDAL installation.
#' @param ask `logical`. If `TRUE` (default) and permanent settings file does 
#'   not exist (see Details), the user is asked whether to create it.
#' @param check_earthdata_login `logical`. If `TRUE` (default), look for 
#'   Earthdata Login credentials in `~/.netrc` and try to download a small 
#'   sample `.hdf` file.
#' 
#' @return 
#' An invisible `list` of **MODIS** options. In addition, the most relevant of 
#' these options are printed to the console. Use [utils::capture.output()] to 
#' prevent this behavior.
#' 
#' @details 
#' These settings are easy to change and take effect immediately! However, 
#' please mind that the 
#' [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
#' does not permit automated write access to the user's file system exempt for 
#' [tempdir()]. Therefore, changes made to [MODISoptions()] remain temporary for
#' the current session unless write access is explicitly granted by the user in 
#' interactive mode, in which case a permanent settings file is created in 
#' `file.path("~/.MODIS_Opts.R")` (per user) or 
#' `file.path(R.home(component = "etc"), '.MODIS_Opts.R')` (system-wide, write 
#' access provided).
#' 
#' Due to similar reasons, 'localArcPath' and 'outDirPath' default to 
#' [tempdir()] and should be changed immediately after loading the package in 
#' order to make downloaded files permanently available. You may also specify a 
#' shared network drive if you have a central MODIS data server. 
#' 
#' If you change default values, consider that your settings have to be valid 
#' for any MODIS product, layer and area!
#' 
#' Please note that in order to download MODIS files from any available server, 
#' you are required to 
#' * register for an Earthdata Login profile at
#'   <https://urs.earthdata.nasa.gov/users/new>, 
#' * fill out mandatory fields therein, and
#' * create a `.netrc` file in your home directory containing the Earthdata 
#'   server address and your login credentials. An automated solution for the 
#'   creation of such a file is provided through [EarthdataLogin()]. 
#' 
#' @author 
#' Matteo Mattiuzzi, Steven Mosher and Florian Detsch
#' 
#' @examples 
#' \dontrun{
#' ## get options
#' MODISoptions()
#' 
#' ## set options
#' lap = "/another/path/to/MODIS_ARC" # 'localArcPath'
#' odp = file.path(lap, "PROCESSED")  # 'outDirPath'
#' 
#' MODISoptions(localArcPath = lap, outDirPath = odp)
#' }
#' 
#' @export MODISoptions
#' @name MODISoptions
MODISoptions <- function(localArcPath, outDirPath, pixelSize, outProj, 
                         resamplingType, dataFormat, gdalPath, MODISserverOrder, 
                         dlmethod, stubbornness, wait, quiet, cellchunk,
                         systemwide = FALSE, save = TRUE, checkTools = TRUE,
                         checkWriteDrivers = TRUE, ask = TRUE, 
                         check_earthdata_login = TRUE)
{
  # This function collects the package options from up to 3 files and creates 
  # the .MODIS_Opts.R file (location depending on systemwide=T/F, see below):
  # 1. package installation directory (factory defaults); 
  # 2. /R/etc/.MODIS_Opts.R for system wide settings (all users of a machine) and 
  # 3. user home "~/.MODIS_Opts.R", for user specific settings. 
  # settings are collected in direction 1-3 and each time overwritten if available
  # The final settings are written in to the user specific file 3.
  # options are not tested here! only generated!
  
  # container for all options
  opts  <- new.env()
  
  ##################################
  # 1. factory defaults -----
  ofl = suppressWarnings(
    system.file("external", "MODIS_Opts.R", package = "MODIS")
  )
  
  if (ofl == "")
    ofl = file.path(getwd(), "inst/external/MODIS_Opts.R")
  
  eval(parse(ofl), envir = opts) 
  
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
    tmpopt = file.path(tempdir(), ".MODIS_Opts.R")
    
    if (any(file.exists(optfile), file.exists(tmpopt))) {
      eval(parse(ifelse(file.exists(optfile), optfile, tmpopt)), envir = opts)
      uo <- TRUE
    }
    
    whose <- 'user'
  }
  
  # if(!uo) {
  #   if(!so & save) {
  #     warning("No MODIS 'user' nor 'systemwide' settings file found. File is created for '",whose,"'-settings in: ",normalizePath(optfile,'/',mustWork=FALSE),sep="")
  #   } else if (!save) {
  #     warning("No MODIS 'user' nor 'systemwide' settings file found, using factory defaults. Use '?MODISoptions' to configure the 'MODIS' package and make settings permanent!")
  #   }
  # }

  #################################
  opt <- as.list(opts)	
  
  # localArcPath
  opt$localArcPath <- correctPath(opt$localArcPath)
  
  if(!missing(localArcPath)) {
    localArcPath <- correctPath(localArcPath) 
  
    if (opt$localArcPath != localArcPath) {
      message("Setting 'localArcPath' to '"
              , normalizePath(localArcPath, "/", FALSE)
              , "'\nIf you already have downloaded some HDF files, "
              , "you can use '?orgStruc' to rearrange them.")
    }
    
    options(MODIS_localArcPathWarned=TRUE)
    opt$localArcPath <- localArcPath
  } else {
    if (length(list.dirs(opt$localArcPath,recursive=FALSE))==0)
    {
      if(!isTRUE(options()$MODIS_localArcPathWarned))
      {
        message("'localArcPath' does not exist and will be created in '"
                , normalizePath(opt$localArcPath, "/", FALSE)
                , "'. Consult '?MODISoptions' if you want to change it!")     
        options(MODIS_localArcPathWarned=TRUE)
      } 
    }
  }
  
  # outDirPath
  opt$outDirPath <- correctPath(opt$outDirPath)
  
  if(!missing(outDirPath))
  {
    outDirPath <- correctPath(outDirPath)
    
    if (!dir.exists(outDirPath))
    {
      message("'outDirPath' does not exist and will be created in '"
              , normalizePath(outDirPath, "/", FALSE), "'.")
    } else if (opt$outDirPath != outDirPath)
    {
      message("'outDirPath' has been changed from '"
              , normalizePath(opt$outDirPath, "/", FALSE)
              , "' to '", normalizePath(outDirPath, "/", FALSE), "'.")
    }
    options(MODIS_outDirPathWarned=TRUE) 
    opt$outDirPath <- outDirPath
  } else
  {
    if (!dir.exists(opt$outDirPath))
    {
      if(!isTRUE(options()$MODIS_outDirPathWarned))
      {
          message("'outDirPath' does not exist and will be created in '"
                  , normalizePath(opt$outDirPath, "/", FALSE)
                  , "'. Consult '?MODISoptions' if you want to change it!")               
        options(MODIS_outDirPathWarned=TRUE)
      }
    }    
  }

  opt$auxPath <- paste0(opt$outDirPath,".auxiliaries/")
  
  if(!missing(dlmethod))
  {
    dlmethod <- tolower(dlmethod)
    stopifnot(
      dlmethod %in% c(
        "internal"
        , if (Sys.info()[["sysname"]] == "Windows") "wininet"
        , "libcurl"
        , "wget"
        , "curl"
        , "auto"
        , "aria2"
      )
    )
    opt$dlmethod <- dlmethod
  }
  
  if(!missing(stubbornness))
  {
    opt$stubbornness <- stubbornness
  }
  
  if (!missing(wait)) opt$wait <- wait
  if (!missing(quiet)) opt$quiet <- quiet
  
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
  }
  opt$gdalPath <- correctPath(opt$gdalPath)

  if(is.null(opt$MODISserverOrder))
  {
    opt$MODISserverOrder <- c("LPDAAC", "LAADS", "NSIDC")
  }
  if (!missing(MODISserverOrder)) {
    
    MODISserverOrder = intersect(
      toupper(
        MODISserverOrder
      )
      , c("LPDAAC", "LAADS", "NSIDC")
    )
    
    if (length(MODISserverOrder) == 0L) {
      stop(
        "Provide valid 'MODISserverOrder', see `?MODISoptions()`."
        , call. = FALSE
      ) 
    }
    
    opt$MODISserverOrder = MODISserverOrder
  }
  
  if (
    (
      is.null(opt$EarthdataLogin) || # check never performed
      isFALSE(opt$EarthdataLogin)    # check performed, but failed
    ) && 
    check_earthdata_login
  ) {
    opt$EarthdataLogin = checkEarthdataLogin(
      opt$dlmethod
      , opt$MODISserverOrder
    )
  }
  
  # checks if the pointed GDAL exists and supports 'HDF4Image' driver.
  if(checkTools)
  {
    # GDAL
    isOk <- checkHdf4Driver()
    if (isOk) 
    {
      opt$gdalOk  <- TRUE
      gdalVersion <- do.call("checkTools", c(list(tool = "GDAL", quiet = TRUE)))$GDAL$version
    } else
    {
      opt$gdalOk  <- FALSE
      gdalVersion <- "Not available. Use 'MODIS:::checkTools('GDAL')' for more information!"
    }
    
    # MRT
    mrt <- do.call("checkTools", c(list(tool = "MRT", quiet = TRUE)))$MRT
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
    gdalVersion <- "Not checked."
    mrtVersion  <- "Not checked."
  }   
  
  if(!missing(dataFormat))
  {
    opt$dataFormat <- dataFormat
  }
  if(is.null(opt$dataFormat))
  {
    opt$dataFormat <- 'GTiff'
  }
  
  if(checkTools & opt$gdalOk & checkWriteDrivers)
  {
    opt$gdalOutDriver <- getGdalWriteDrivers()
  }
  
  if(!missing(cellchunk))
  {
    opt$cellchunk <- cellchunk
  }
  
  #########
  
  if (save) {
    
    #  let user decide whether to make settings permanent
    answer = if (ask) {
      if (!file.exists(optfile)) {
        readline(paste0("File '", optfile, "' does not exist. Create it now to "
                        , "make settings permanent? [y/n]: "))
      } else "y"
    } else "n"
      
    filename = file(ifelse(tolower(answer) %in% c("y", "yes"), optfile, tmpopt)
                    , open = "wt")

    write(paste0('# This file contains ', whose,' default values for the R package \'MODIS\'.'), filename)
    write('# version 1.1.1', filename)
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
    
    write(paste0('EarthdataLogin   <- ', ifelse(is.null(opt$EarthdataLogin), FALSE, opt$EarthdataLogin)), filename)
    if(length(opt$MODISserverOrder)>=2)
    {
      write(paste0('MODISserverOrder <- c(\'',paste(opt$MODISserverOrder,collapse="', '"),'\')' ), filename)
    } else
    {
      write(paste0('MODISserverOrder <- \'',opt$MODISserverOrder,'\'' ), filename)
    }
    write(paste0('dlmethod         <- \'',opt$dlmethod,'\'' ), filename)
    write(paste0('stubbornness     <- \'',opt$stubbornness,'\''), filename)
    write(paste0('wait             <- ',opt$wait), filename)
    write(paste0('quiet            <- ',opt$quiet), filename)
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
    write('# 4) Defaults related to raster package:', filename)	 
    write('# Cellchunk: Comparable with chunksize in ?rasterOption.',filename)
    write('# But as no effect was found in adapting chunksize,', filename)	
    write('# MODIS applies its own variant:minrows <- max(floor(cellchunk/ncol(x)),1) blockSize(x,minrows=minrows).', filename)	
    write('# On a reasonable working station you can easily increase this to 500000, set 1 for raster defaults', filename)	
    write(paste0('cellchunk <- ',opt$cellchunk), filename)
    write('  ', filename)    
    write('#########################', filename)
    write('# 5.) Set path to GDAL _bin_ directory', filename)
    write('# Optional, used to relate writable sf::st_drivers("raster") to file extensions for non-standard formats.', filename)        
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
  
  cat('\nSTORAGE:\n')
  cat('_______________\n')
  cat('localArcPath :', normalizePath(opt$localArcPath,"/",FALSE), '\n' )
  cat('outDirPath   :', normalizePath(opt$outDirPath,"/",FALSE), '\n\n\n')
  
  cat('DOWNLOAD:\n')
  cat('_______________\n')
  cat('EarthdataLogin   :', opt$EarthdataLogin, "\n")
  cat('MODISserverOrder :', paste(opt$MODISserverOrder,collapse=", "),'\n')
  cat('dlmethod         :', opt$dlmethod,'\n')
  cat('stubbornness     :', opt$stubbornness,'\n')
  cat('wait             :', opt$wait, "\n")
  cat('quiet            :', opt$quiet, "\n\n\n")
  
  cat('PROCESSING:\n')
  cat('_______________\n')
  cat('GDAL           :', gdalVersion, '\n')
  cat('MRT            :', mrtVersion, '\n')
  cat('pixelSize      :', opt$pixelSize, '\n')
  cat('outProj        :', opt$outProj, '\n')
  cat('resamplingType :', opt$resamplingType, '\n')
  cat('dataFormat     :', opt$dataFormat, '\n')
  cat('cellchunk      :', opt$cellchunk,'\n\n\n')
  
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
  return(invisible(opt))
}


