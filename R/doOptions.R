
# this function handles the parameter resamplingTpye and must be placed inside runMrt() and runGdal()
checkResamplingType <- function(resamplingType,tool,quiet=FALSE) 
{
  if (missing(resamplingType))
  {
    resamplingType <- "near"
  }
  
  resamplingType <- trim(tolower(as.character(resamplingType)))
  tool           <- toupper(tool)
  
  if (!tool %in% c("GDAL","MRT"))
  {
    stop("Unknown 'tool'. Allowed are 'MRT' or 'GDAL'")
  }
  
  if ("GDAL" %in% tool)
  {
    if (resamplingType %in% c("nn","cc","bil"))
    {
      if(resamplingType=="nn")
      {
        resamplingType <- "near"
      }
      if(resamplingType=="cc")
      {
        resamplingType <- "cubic"
      }
      if(resamplingType=="bil")
      {
        resamplingType <- "bilinear"
      }
    } else
    {
      # for efficiency gdv should be stored as variable
      gdv <- checkTools('GDAL',quiet=TRUE)$GDAL$vercheck
      
      if (gdv[2] < 10 & resamplingType %in% c("average","mode"))
      {
        stop("resamplingType= 'average' and 'mode' requires GDAL >= 1.10.0")
      }
    }
  }
  
  if (tool=="MRT")
  {
    if(resamplingType %in% c("near","nn"))
    {
      resamplingType <- "nn"
    } else if(resamplingType %in% c("cc","cubic"))
    {
      resamplingType <- "cc"
    } else if(resamplingType %in% c("bil","bilinear"))
    {
      resamplingType <- "bil"
    } else if (resamplingType %in% c("cubicspline","lanczos"))
    {
      if(!quiet)
    {
      warning(resamplingType," resamling is only supported by GDAL, not by MRT tool. If you use MRT 'near' is used insead")
    }
    resamplingType='nn'
    } else
    {
      if(!quiet)
      {
        warning(resamplingType," not supported by 'MRT' using 'NN'")
      }
      resamplingType='nn'
    }
  }
  if (resamplingType %in% c("cc","bil","bilinear","cubic","cubicspline","lanczos"))
  {    
    if(!quiet)
    {
      warning("By not using resamplingType='near'/'nn' some SDS become useless (ie all bit encoded Quality SDS's, or 'day of the year' SDS's). It is strongly recommanded to use resamplingType='near'!")
    }
  }
  
  if (tool=="MRT")
  {
    toupper(resamplingType)
  } else
  {
    tolower(resamplingType)
  }    
  return(resamplingType)
}

# checks validity of outProj and returns for tool="MRT" the short name (see mrt manual) and in case of "GDAL" the prj4 string!
checkOutProj <- function(proj, tool, quiet=FALSE)
{
  tool <- toupper(tool)
  if (!tool %in% c("GDAL", "MRT"))
  {
    stop("checkOptProj Error: Unknown 'tool'. Allowed are 'MRT' or 'GDAL'")
  }
  
  if(proj=="asIn") # lot of troubles because of this!
  {
    return(proj)
  }
  
  # this is here because we could think in a conversion between GDAL and MRT inputs! (the available in MRT is the limiting factor)
  MRTprojs <- matrix(byrow=T,ncol=2,
  c("AEA", "Albers Equal Area", "ER", "Equirectangular", "GEO", "Geographic", 
  "IGH", "Interrupted Goode Homolosine", "HAM", "Hammer", "ISIN", "Integerized Sinusoidal", 
  "LA", "Lambert Azimuthal Equal Area", "LCC", "Lambert Conformal Conic", 
  "MERCAT", "Mercator", "MOL", "Molleweide", "PS", "Polar Stereographic", 
  "SIN", "Sinusoidal", "TM", "Transverse Mercator", "UTM", "Universal Transverse Mercator"),
  dimnames=list(NULL,c("short","long")))

  if (tool=="GDAL") # EPRS:xxxx or xxxx or "+proj=sin...." 
  { # EPSGinfo is lazy loaded (see: minorFuns.R)
    require(rgdal)
    
    inW <- getOption("warn")
    on.exit(options(warn=inW))
    options(warn=-1)
    
    if(toupper(proj) %in% toupper(MRTprojs))
    {
      if (toupper(proj) %in% c("GEO","GEOGRAPHIC"))
      { 
          proj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")@projargs
      } else if (toupper(proj) %in% c("SIN","SINUSOIDAL"))
      {
          proj <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")@projargs
      } else
      {
        stop("Could not convert 'outProj' argunemt",proj, "to a sp:::CRS compatible string!")
      }
    } else if(!is.na(as.numeric(proj)))
    {
      proj <- CRS(paste0("+init=epsg:",proj))@projargs
    } else if(length(grep(proj,pattern="EPSG:",ignore.case=TRUE))==1)
    {
      proj <- CRS(gsub(proj,pattern="^EPSG:",replacement="+init=epsg:", ignore.case=TRUE))@projargs      
    } else if (inherits(proj,"CRS"))
    {
      proj <- proj@projargs
    } else
    {
      options(warn=inW) # here warning is usefull
      proj <- CRS(proj)@projargs
    }
    return(proj)
  }
  
  if (tool == "MRT")
  {
    ind <- grep(MRTprojs,pattern=paste("^",proj,"$",sep=""),ignore.case=TRUE)
    
    if(length(ind)==0)
    {
      cat("'outProj' must be one of:\n")
      return(MRTprojs)
    } else
    {
      
      if(ind > nrow(MRTprojs)) # catch short name
      {
        indL <- ind
        ind  <- ind-nrow(MRTprojs)
      } else
      {
        indL <- ind+nrow(MRTprojs)
      }
      
      return(list(short = MRTprojs[ind],long = MRTprojs[indL]))
    }
  }
}

# returns 0 if a given GDAL supports HDF4 else 1 
checkGdalDriver <- function(path=NULL)
{
  inW <- getOption("warn")
  on.exit(options(warn=inW))
  options(warn=-1)
  
  path <- correctPath(path)
  
  cmd <- paste0(path,'gdalinfo --formats')
  
  if(.Platform$OS=="windows")
  {
    driver <- try(shell(cmd,intern=TRUE),silent=TRUE)
  } else
  { 
    driver <- try(system(cmd,intern=TRUE), silent=TRUE)
  }
    
  if (class(driver) == "try-error")
  {
    options(warn=inW)
    warning("No gdal installation found please install 'gdal' on your system first!")
    return(FALSE)
  }
    
  if(length(grep(driver,pattern="HDF4"))==0)
  {
    return(FALSE)
  } else
  {
    return(TRUE)
  }
}

combineOptions <- function(...) 
{
  opts <- options()
  opts <- opts[grep(names(opts),pattern="^MODIS_*.")] # isolate MODIS_opts
  
  if(length(opts)==0) # if nothing available look for initial options
  {
    if(!file.exists("~/.MODIS_Opts.R"))
    {
      warning("MODIS_Opts file not found, run '?MODISoptions' to see and set permanent package defaults!\n")
    }
    MODISoptions(save=FALSE,quiet=TRUE)    
    opts <- options() # collects all options
    opts <- opts[grep(names(opts),pattern="^MODIS_*.")] # isolate MODIS_opts
  }
  names(opts) <- gsub(names(opts),pattern="MODIS_",replacement="") # convert names to function arg style 
  
  Fopts <- list(...) # collects fun args
  if (length(Fopts)==0)
  {
    Fopts <- NULL
  }
  
  opts <- c(Fopts, opts[(!names(opts) %in% names(Fopts))])
  return(opts)
}


