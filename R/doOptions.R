
# this function handles the parameter resamplingTpye and must be placed inside runMrt() and runGdal()
checkResamplingType <- function(resamplingType,tool,quiet=FALSE) 
{
  if (missing(resamplingType))
  {
    resamplingType <- "near"
  }
  
  resamplingType <- raster::trim(tolower(as.character(resamplingType)))
  tool           <- toupper(tool)
  
  if (!tool %in% c("GDAL","MRT"))
  {
    stop("Unknown 'tool'. Allowed are 'MRT' or 'GDAL'")
  }
  
  if ("GDAL" %in% tool)
  {
    if (resamplingType %in% c("nn","cc","bil"))
    {
      resamplingType = switch(
        resamplingType
        , "nn" = "near"
        , "cc" = "cubic"
        , "bil" = "bilinear"
      )
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
checkOutProj <- function(proj, tool = c("GDAL", "MRT"), quiet=FALSE
                         # , zone = NULL
)
{
  tool <- toupper(tool)
  tool <- match.arg(tool)

  if (proj == "asIn" || inherits(proj, "crs")) # lot of troubles because of 'asIn'!
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
  
  if (tool=="GDAL") # EPSG:xxxx or xxxx or "+proj=sin...." 
  { 
    
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
        proj <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")@projargs
        # } else if (toupper(proj) %in% c("UTM", "Universal Transverse Mercator")) 
        # {
        #   if (is.null(zone)) {
        #     stop("An UTM zone needs to be specified when `outProj = 'UTM'`.")
        #   }
        #   
        #   hemisphere = if (zone < 0) " +south " else " "
        #   proj = paste0("+proj=utm "
        #                , "+zone=", zone
        #                , hemisphere
        #                # , if (datum != "NODATUM") paste0("+datum=", datum)
        #                , "+ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      } else
      {
        stop("Could not convert 'outProj' argument",proj, "to a sp::CRS compatible string!")
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
      options(warn=inW) # here warning is useful
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

combineOptions <- function(checkTools = TRUE, ...) 
{
  opts <- options()
  opts <- opts[grep(names(opts),pattern="^MODIS_*.")] # isolate MODIS_opts
  
  if(length(opts)==0) # if nothing available look for initial options
  {
    # if(!file.exists("~/.MODIS_Opts.R")) {
    #   warning("File '~/.MODIS_Opts.R' not found. "
    #           , "Run MODISoptions() to make settings permanent!")
    # }
    requireNamespace("MODIS", quietly = TRUE)
    jnk = capture.output(
      MODISoptions(
        save = FALSE
        , checkTools = checkTools
        , check_earthdata_login = FALSE
      )
    )
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


### check output utm zone number passed to runMrt() ----

checkUTMZone = function(zone = NULL) {
  if (!is.null(zone)) {
    zone = suppressWarnings(as.integer(zone))
    if (is.na(zone) | zone < -60 | zone > 60 | zone == 0) {
      warning("Output UTM zone needs to be a non-zero integer between -60 and 60, autodetecting ...")
      zone = NULL
    }
  }
  
  return(zone)
}
