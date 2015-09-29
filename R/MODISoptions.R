MODISoptions <- function(localArcPath, outDirPath, pixelSize, outProj, resamplingType, dataFormat, gdalPath, MODISserverOrder, dlmethod, stubbornness, systemwide = FALSE, quiet=FALSE, save=TRUE, checkPackages=TRUE)
{
  # This function collects the package options from up to 3 files and creates the .MODIS_opts.R file (location depending on systemwide=T/F, see below):
  # 1. package installation directory (factory defaults); 
  # 2. /R/etc/.MODIS_opts.R for system wide settings (all users of a machine) and 
  # 3. user home "~/.MODIS_opts.R", for user specific settings. 
  # settings are collected in direction 1-3 and each time overwritten if available
  # The final settings are written in to the user specific file 3.
  # options are not tested here! only generated!
  
  # debug: systemwide = FALSE; quiet=FALSE; save=TRUE; checkPackages=TRUE
  if(checkPackages)
  {
    # check if all suggested packages are installed:
    suggestedPackages <- checkDeps()
  } else
  {
    suggestedPackages <- "run 'MODISoptions(checkPackages=TRUE)' for further details"
  }
  # container for all options
  opts  <- new.env()
  
  ##################################
  # 1. factory defaults
  eval(parse(file.path(find.package("MODIS"), "external", "MODIS_Opts.R")),envir=opts) 
  
  # 2. system wide
  sysopts <- paste(R.home(component="etc"), '/', '.MODIS_Opts.R', sep='')
  so      <- FALSE
  
  if (file.exists(sysopts))
  {
    eval(parse(sysopts),envir=opts)
    so <- TRUE
  }
  
  # 3. user specific
  optfile <- file.path("~/.MODIS_Opts.R",fsep="/")
  uo      <- FALSE
  
  if(systemwide)
  {
    if(!file.create(sysopts,showWarnings=FALSE))
    {
      stop("You do not have write permission in ",R.home(component="etc")," to create/change 'systemwide' MODIS options. Set 'systemwide=FALSE' for single user settings or start R as root/admin and run again 'MODISoptions'!")
    }
    optfile <- sysopts
    whose   <- 'systemwide'
  } else
  {
    if (file.exists(optfile))
    {   
      eval(parse(optfile),envir=opts)
      uo <- TRUE
    }
    whose <- 'user'
  } 
  
  if(!uo)
  {
    if(!so & save)
    {
      warning("No MODIS 'user' nor 'systemwide' settings file found. File is created for '",whose,"'-settings in: ",normalizePath(optfile,'/',mustWork=FALSE),sep="")
    } else if (!save)
    {
      warning("No MODIS 'user' nor 'systemwide' settings file found, using factory defaults. Use '?MODISoptions' to configure the 'MODIS' package and make settings permanent!")
    }
  }
  #################################
  opt <- as.list(opts)	
  
  # localArcPath
  opt$localArcPath <- correctPath(opt$localArcPath)
  
  if(!missing(localArcPath))
  {
    localArcPath <- correctPath(localArcPath) 
  
    if (opt$localArcPath != localArcPath)
    {
      message("Setting 'localArcPath' to '", normalizePath(localArcPath,"/",FALSE),"'\nIf you already have downloaded some HDF-files to '",normalizePath(opt$localArcPath,"/",FALSE) ,"' you can use '?orgStruc()' to re-arrange your HDF-data!")
    }
    options(MODIS_localArcPathWarned=TRUE)
    opt$localArcPath <- localArcPath
  } else
  {
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
    opt$MODISserverOrder <- c("LPDAAC","LAADS")
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
  if(checkPackages)
  {
    # GDAL
    isOk <- checkGdalDriver(path=opt$gdalPath)
    if (isOk) 
    {
      opt$gdalOk  <- TRUE
      gdalVersion <- checkTools(tool="GDAL",quiet=TRUE)$GDAL$version
    } else
    {
      opt$gdalOk  <- FALSE
      gdalVersion <- "Not available. Use 'MODIS:::checkTools('GDAL')' for more information!"
    }
    
    # MRT
    mrt <- checkTools(tool="MRT",quiet=TRUE)$MRT
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
  
  if(checkPackages & opt$gdalOk)
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
    
    cat('DEPENDENCIES:\n')
    cat('_______________\n')
    cat(suggestedPackages,'\n\n')
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


