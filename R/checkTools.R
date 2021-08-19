checkTools = function(
  tool = c("MRT", "GDAL", "wget", "curl")
  , quiet = FALSE
) {

  tool = tolower(tool)

  iw = options()$warn
  options(warn = -1)
  on.exit(options(warn = iw))


  ### . mrt ----

  MRT = if ("mrt" %in% tool) {
    checkMrt(quiet = quiet)
  }


  ### . gdal ----

  GDAL = if ("gdal" %in% tool) {
    checkGdal(quiet = quiet)
  }


  ### . wget ----

  WGET = if ("wget" %in% tool) {
    checkWget()
  }


  ### . curl ----

  CURL = if ("curl" %in% tool) {
    checkCurl()
  }


  return(
    invisible(
      list(
        GDAL = GDAL
        , MRT = MRT
        , WGET = WGET
        , CURL = CURL
      )
    )
  )
}


checkMrt = function(quiet = FALSE) {
  MRT   <- FALSE
  mrtH  <- normalizePath(Sys.getenv("MRT_HOME"), winslash="/", mustWork = FALSE)
  mrtDD <- normalizePath(Sys.getenv("MRT_DATA_DIR"), winslash="/", mustWork = FALSE)

  if (!quiet)
  {
    cat("Checking availability of MRT:\n")
  }

  if(mrtH=="" & !quiet)
  {
    cat("  'MRT_HOME' not set/found! MRT is NOT enabled! See: 'https://lpdaac.usgs.gov/tools/modis_reprojection_tool'\n")
  } else
  {
    if (!quiet)
    {
      cat("  'MRT_HOME' found:", mrtH,"\n")
    }
    if (mrtDD=="" & !quiet)
    {
      cat("  'MRT_DATA_DIR' not set/found! MRT is NOT enabled! You need to set the path, read in the MRT manual! 'https://lpdaac.usgs.gov/tools/modis_reprojection_tool'\n")
    } else
    {
      if (!quiet)
      {
        cat("  'MRT_DATA_DIR' found:",mrtDD,"\n")
        cat("   MRT enabled, settings are fine!\n")
      }
      MRT <- TRUE
    }
  }
  if(MRT)
  {
    if(file.exists(paste0(mrtH,"/doc/ReleaseNotes.txt")))
    {
      x <- file(paste0(mrtH,"/doc/ReleaseNotes.txt"),open="rt")
      v <- readLines(x)
      v <- v[(grep(v,pattern="------*")-1)]
      v <- v[grep(v,pattern="Version ")][1]
      close(x)
    } else
    {
      v <- "Enabled"
    }
  } else
  {
    v <- "Version not determined"
  }
  list(MRT=MRT,version=v)
}


checkGdal = function(quiet = FALSE) {
  esv = sf::sf_extSoftVersion()
  if (!quiet) {
    cat(
      "Checking availability of GDAL:"
      , paste("  OK, GDAL", esv["GDAL"], "found!")
      , sep = "\n"
    )
  }
  list(
    GDAL = TRUE # required by sf
    , version = unname(esv["GDAL"])
    , vercheck = as.integer(strsplit(esv["GDAL"], "\\.")[[1]])
  )
}


checkWget = function() {
  WGET = FALSE
  wgetOK = try(system("wget --version", intern = TRUE), silent = TRUE)

  wgettext = if (!inherits(wgetOK, "try-error")) {
    WGET = TRUE
    regmatches(wgetOK[1], regexpr("GNU Wget [[:digit:]\\.]+", wgetOK[1]))
  } else ""

  list(WGET = WGET, version = wgettext)
}


checkCurl = function() {
  CURL = FALSE
  curlOK = try(system("curl --version", intern = TRUE), silent = TRUE)

  curltext = if (!inherits(curlOK, "try-error")) {
    CURL = TRUE
    regmatches(curlOK[1], regexpr("curl [[:digit:]\\.]+", curlOK[1]))
  } else ""

  list(CURL = CURL, version = curltext)
}


checkHdf4Driver = function() {
  avl = "HDF4" %in% sf::st_drivers(what = "raster")$name
  if (!avl) {
    warning("HDF4 driver seems to be lacking, please install GDAL with HDF4 support.")
  }
  return(avl)
}


checkGdalWriteDriver = function(dataFormat) {
  if (toupper(dataFormat) == 'RAW BINARY') {
    stop("dataFormat = '", dataFormat, "' is MRT specific, "
         , "run MODIS:::getGdalWriteDrivers() for GDAL supported write formats.")
  }
  nms = as.character(getGdalWriteDrivers()$name)
  avl = toupper(dataFormat) == toupper(nms)
  if (!any(avl)) {
    stop("dataFormat = '", dataFormat, "' not recognized by GDAL, "
         , "run MODIS:::getGdalWriteDrivers() for supported write formats.")
  }
  return(nms[avl])
}


checkMrtWriteDriver = function(dataFormat) {
  nms = c('raw binary', 'hdf-eos', 'hdf4image','gtiff', 'geotiff')
  avl = tolower(dataFormat) == nms
  if(!any(avl)) {
    stop("dataFormat = '", dataFormat, "' not recognized by MRT, "
         , "choose one of c('raw binary', 'HDF-EOS', 'GeoTiff')."
    )
  }
  switch(
    nms[avl]
    , "raw binary" = ".hdr"
    , "hdf-eos" = ".hdf"
    , "hdf4image" = ".hdf"
    , "gtiff" = ".tif"
    , "geotiff" = ".tif"
  )
}
