#' Process MODIS HDF with GDAL
#' 
#' @description 
#' Downloads MODIS grid files from archive (HTTP or local) and processes them.
#' 
#' @param product `character`, see [getProduct()].
#' @param collection `character` or `integer`, see [getCollection()].
#' @param begin,end `Date` or `character`. Begin and end date of MODIS time 
#'   series, see [transDate()].
#' @param extent Extent information, defaults to 'global'. See [getTile()].
#' @param tileH,tileV `numeric` or `character`. Horizontal and vertical tile 
#'   number, see [getTile()].
#' @param SDSstring `character`, see [getSds()].
#' @param job `character`. Name of the current job for the creation of the 
#'   output folder. If not specified, it is created in 
#'   'PRODUCT.COLLECTION_DATETIME'.
#' @param checkIntegrity,forceDownload `logical`, see [getHdf()].
#' @param overwrite `logical`, defaults to `FALSE`. Determines whether or not to
#'   overwrite existing SDS output files.
#' @param maskValue If `NULL` (default), i.e. not explicitly set, the per-band
#'   `NoData Value` is taken into account. If not `NULL`, a vector of masking 
#'   values with each value corresponding to a single band in 'SDSstring'. This 
#'   can include `"None"` to ignore intrinsic no-data settings on the source
#'   data set. See also
#'   <https://gdal.org/programs/gdalwarp.html#cmdoption-gdalwarp-srcnodata> for 
#'   details.
#' @param ... Additional arguments passed to [MODISoptions()], e.g. 'wait'. 
#'   Permanent settings for these arguments are temporarily overridden.
#' 
#' @return 
#' A `list` of the same length as 'product'. Each product slot either holds a 
#' sub-`list` of processed dates which, for each time step, includes the 
#' corresponding output files as `character` objects or, if no files could be 
#' found for the specified time period, a single `NA`.
#' 
#' @details
#' * `outProj, pixelSize, resamplingType, dataFormat, localArcPath, outDirPath`: 
#'   See [MODISoptions()].
#' * `blockSize`: integer. If `NULL` (default), the stripe size is set by GDAL. 
#'   Basically it is the `-co BLOCKYSIZE=` parameter. See 
#'   <https://gdal.org/frmt_gtiff.html>.
#' * `compression` logical. If `TRUE` (default), compress data with the lossless
#'   LZW compression with `predictor=2`. See <https://gdal.org/frmt_gtiff.html>.
#' 
#' [runGdal()] uses numerous **MODIS** functions under the hood, see the linked 
#' functions in Arguments for details and inputs.
#' 
#' If 'extent' is a `Raster*` object, the output has exactly the same extent, 
#' pixel size, and projection.
#' If 'extent' is a **sp** or **sf** object, the output has exactly the same 
#' extent and projection except for point geometries with length 1 (i.e. a 
#' single point) where only the projection is inherited.
#' If 'tileH' and 'tileV' are used (instead of 'extent') to define the area of 
#' interest, and 'outProj' and 'pixelSize' are `"asIn"`, the result is only 
#' converted from multi-layer HDF to 'dataFormat', default `"GTiff"`.
#' 
#' @author 
#' Matteo Mattiuzzi, Florian Detsch
#' 
#' @seealso 
#' [getHdf()], [runMrt()].
#' 
#' @examples 
#' \dontrun{
#' # LST in Austria
#' runGdal( product="MOD11A1", extent="austria", begin="2010001", end="2010005", SDSstring="101")
#' 
#' # LST with interactive tile selection
#' runGdal( product="MOD11A1", begin="2010001", end="2010005", SDSstring="101")
#' 
#' ### outProj examples
#' # LST of Austria warped to UTM 34N (the three different possibilites to specify "outProj")
#' # to find am EPSG or prj4 you may use: prj <- make_EPSG() See  
#' runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
#'          SDSstring="101", outProj="EPSG:32634")
#' 
#' runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
#'          SDSstring="101", outProj=32634)
#' 
#' runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
#'          SDSstring="101", outProj="+proj=utm +zone=34 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#' 
#' ### resamplingType examples
#' runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
#'          SDSstring="1", resamplingType="lanczos", outProj="32634", pixelSize=100)
#' 
#' ### processing entire tiles and keeping Sinusoidal projection
#' # This corresponds to a format conversion (eos-hdf04 to Geotiff) and 
#' # layer extraction (multi-layer to single layer)
#' runGdal( job="LSTaustria", product="MOD11A1", tileH=18:19,tileV=4, begin="2010001", end="2010005",
#'          SDSstring="1", outProj="asIn")
#' 
#' }
#' 
#' @export runGdal
#' @name runGdal
runGdal <- function(product, collection=NULL, 
                    begin=NULL, end=NULL, 
                    extent, tileH, tileV, 
                    SDSstring=NULL, job=NULL, checkIntegrity=TRUE, 
                    forceDownload=TRUE, overwrite = FALSE, maskValue=NULL, ...)
{
    opts <- combineOptions(...)

    if(!opts$gdalOk) {
        stop("GDAL not installed or configured, read in '?MODISoptions' for help")
    }
    
    # absolutely needed
    product <- getProduct(product, quiet=TRUE, collection = collection)
    
    # optional and if missing it is added here:
    tLimits     <- transDate(begin=begin,end=end)
    
    dataFormat <- toupper(opts$dataFormat) 

    if(dataFormat == 'HDF-EOS')
    {
        dataFormat <- "HDF4IMAGE"
    } else if(dataFormat == 'GEOTIFF')
    {
        dataFormat <- "GTIFF"
    }
    
    if(is.null(opts$gdalOutDriver))
    {
        opts$gdalOutDriver = getGdalWriteDrivers()
        options("MODIS_gdalOutDriver"=opts$gdalOutDriver) # save for current session
    }
    
    dataFormat = checkGdalWriteDriver(dataFormat)
    xtn = getExtension(dataFormat)

    #### settings with messages
    # output pixel size in output proj units (default is "asIn", but there are 2 chances of changing this argument: pixelSize, and if extent comes from a Raster* object.
    
    ## pass missing args on to getTile() (see 
    ## https://stackoverflow.com/questions/31557805/passing-missing-argument-from-function-to-function-in-r)
    args = as.list(match.call())
    args[[1]] <- NULL # remove first list element, it's the function call
    args = args[names(args) %in% c("extent", "tileH", "tileV")]
    names(args)[names(args) == "extent"] = "x"
    
    if (missing(extent) || !inherits(extent, "MODISextent")) {
      extent = if (product@TYPE[1] == "Tile" || 
                   (product@TYPE[1] == "CMG" && 
                    any(!missing(extent), all(!missing(tileH), !missing(tileV))))) {
        do.call(
          getTile
          , append(args, list(outProj = opts$outProj, pixelSize = opts$pixelSize))
          , envir = parent.frame()
        )
      }
    }

    
    ### GDAL command line arguments -----

    ## obligatory arguments
    t_srs <- do.call(OutProj, append(list(product = product, extent = extent), opts))
    tr <- do.call(PixelSize, append(list(extent = extent), opts))
    rt <- do.call(ResamplingType, opts)
    s_srs <- InProj(product)                # inProj
    te <- TargetExtent(extent,              # targetExtent
                       outProj = t_srs)

    ## non-obligatory arguments (GTiff blocksize and compression, see
    ## https://gdal.org/frmt_gtiff.html)
    bs <- do.call(BlockSize, opts)
    cp <- do.call(OutputCompression, opts)
    co <- c(cp, bs)
    
    
    ### PRODUCT PROCESSING ====
    
    lst_product <- vector("list", length(product@PRODUCT))
    for (z in seq_along(product@PRODUCT)) {
      # z=1
      todo <- paste(product@PRODUCT[[z]], product@CCC[[product@PRODUCT[z]]], sep = ".")
      
      if(z==1)
      {
        if (is.null(job)) {
          job <- paste0(todo[1],"_",format(Sys.time(), "%Y%m%d%H%M%S"))    
          cat("Output directory = ",gsub("//", "/", paste0(normalizePath(opts$outDirPath,"/",mustWork=FALSE),"/",job))," (no 'job' name specified, generated (date/time based))\n")
        } else {
          cat("Output Directory = ",gsub("//", "/", paste0(normalizePath(opts$outDirPath,"/",mustWork=FALSE),"/",job)),"\n")
        }
        cat("########################\n")
        
        outDir <- file.path(opts$outDirPath,job,fsep="/")
        outDir <- gsub("//", "/", outDir)
        dir.create(outDir,showWarnings=FALSE,recursive=TRUE)
      }
      
      lst_todo <- vector("list", length(todo))
      for (u in seq_along(todo)) {
        # u=1
        ftpdirs      <- list()
        
        server = product@SOURCE[[z]]
        
        jnk = strsplit(todo[u],"\\.")[[1]]
        prodname = jnk[1] 
        coll     = jnk[2]
        
        # cycle through available servers
        idx = stats::na.omit(
          match(
            opts$MODISserverOrder
            , server
          )
        )
        
        struc = try(
          log("e")
          , silent = TRUE
        )
        
        n = 1L
        for (i in server[idx]) {
          jnk = utils::capture.output(
            struc <- try(
              getStruc(
                product = prodname
                , collection = coll
                , begin = tLimits$begin
                , end = tLimits$end
                , server = i
              )
              , silent = TRUE
            )
          )
          
          if (!inherits(struc, "try-error")) {
            opts$MODISserverOrder = server[idx][n:length(idx)]
            break
          }
          
          n = n + 1L
        }
        
        if (inherits(struc, "try-error")) {
          stop(
            sprintf(
              paste0(
                "'%s.%s' is not available on %s or the server is currently not "
                , "reachable. If applicable, try another server or collection."
              )
              , prodname
              , coll
              , paste(
                opts$MODISserverOrder
                , collapse = ", "
              )
            )
            , call. = FALSE
          )
        }
        
        ftpdirs[[1]] = as.Date(
          struc$dates
        )
        
        avDates <- ftpdirs[[1]]
        avDates <- avDates[avDates!=FALSE]
        avDates <- avDates[!is.na(avDates)]        
        
        sel     <- as.Date(avDates)
        
        st = correctStartDate(tLimits$begin, sel, prodname, quiet = opts$quiet)
        us = sel >= st & sel <= tLimits$end
        
        if (sum(us,na.rm=TRUE)>0)
        {
          avDates <- avDates[us]
          
          lst_ofile <- as.list(rep(NA, length(avDates)))
          for (l in seq_along(avDates)) { 
            # l=1
            files <- unlist(
              getHdf(product = prodname, collection = coll
                     , begin = avDates[l], end = avDates[l]
                     , extent = extent, checkIntegrity = checkIntegrity
                     , stubbornness = opts$stubbornness, quiet = opts$quiet
                     , MODISserverOrder = opts$MODISserverOrder
                     , forceDownload = forceDownload, wait = opts$wait)
            )
            
            files <- files[basename(files)!="NA"] # is not a true NA so it need to be like that na not !is.na()
            # silently remove empty or invalid files from list
            if (checkIntegrity) files <- files[checkIntegrity(files)]

            if(length(files)>0)
            {
              SDS = lapply(
                files
                , getSds
                , SDSstring = SDSstring
              )

              NAS <- getNa(SDS[[1]]$SDS4gdal)
              
              if (!is.null(maskValue)) {
                if ((l1 <- length(maskValue)) == 1L & (l2 <- length(NAS)) > 1L) {
                  maskValue = rep(
                    maskValue
                    , l2
                  )
                }
                
                if (!l1 %in% c(1L, l2)) {
                  stop(
                    sprintf(
                      paste(
                        "'maskValue' length needs to be 1 or match 'SDSstring'"
                        , "(i.e. %s), got %s."
                      )
                      , l2
                      , l1
                    )
                    , call. = FALSE
                  )
                }
              }

              ## loop over sds
              ofiles <- character(length(SDS[[1]]$SDSnames))
              
              for (i in seq_along(SDS[[1]]$SDSnames)) {
                # i=1
                outname <- paste0(paste0(strsplit(basename(files[1]),"\\.")[[1]][1:2],collapse="."),
                   ".", gsub(SDS[[1]]$SDSnames[i],pattern=" ",replacement="_"), xtn)
                  
                ## system independent arguments and files required for gdal operations
                ofile <- paste0(outDir, '/', outname)

                # rpl = system.file("gdal", "val_repl.py", package = "MODIS")
                # Sys.chmod(rpl, mode = "0700", use_umask = TRUE)
                
                gdalSDS <- sapply(SDS,function(x){x$SDS4gdal[i]}) # get names of layer 'o' of all files (SDS)
                
                naID <- which(SDS[[1]]$SDSnames[i] == names(NAS))
                srcnodata = if (!is.null(maskValue)) {
                  maskValue[i]
                } else if (length(naID) > 0) {
                  NAS[[naID]]
                }

                if (!file.exists(ofile) || overwrite) {
                  
                  if (file.exists(ofile)) {
                    jnk = file.remove(ofile)
                  }
                  
                  ## create first set of gdal options required by subsequent step
                  lst = list(dataFormat, co, rt, srcnodata)
                  names(lst) = paste0(
                    "-"
                    , c("of", "co", "r", "srcnodata")
                  )
                  lst = Filter(Negate(is.null), lst)
                  
                  params = character()
                  for (j in seq(lst)) {
                    params = c(params, names(lst)[j], lst[[j]])
                  }
                  
                  qt = !is.null(opts$quiet) && opts$quiet
                  
                  ## if required, adjust pixel size and/or target extent
                  if (is.null(tr) | (!is.null(extent@target) & t_srs == s_srs)) {
                    
                    # extract whole tile
                    sf::gdal_utils(
                      util = "warp"
                      , source = gdalSDS
                      , destination = ofile
                      , options = c(
                        params
                        , "-overwrite"
                        , if (qt) {
                          "-multi"
                        }
                      )
                      , quiet = qt
                    )
                    
                    # if '-ts' is missing, convert 'asIn' to actual pixel size
                    tmp = raster::raster(ofile)  
                    
                    if (is.null(tr)) {
                      if (t_srs != s_srs) {
                        tmp = raster::projectExtent(tmp, crs = t_srs)
                      } 
                      
                      tr = raster::res(tmp)
                    }
                    
                    # if 'outProj == "asIn"', make sure input and output grid
                    # alignment is identical
                    if (!is.null(extent@target$extent) & t_srs == s_srs) {
                      tmp = raster::crop(tmp, extent@target$extent, snap = "out")
                      te = as.character(sf::st_bbox(tmp))
                    }
                    
                    rm(tmp)
                  }
                  
                  ## extract layers
                  lst = c(lst, list("-t_srs" = if (t_srs != s_srs) t_srs, "-te" = te, "-tr" = tr))
                  lst = Filter(Negate(is.null), lst)
                  
                  for (j in (j+1):length(lst)) {
                    params = c(params, names(lst)[j], lst[[j]])
                  }
                  
                  jnk = file.remove(ofile)
                  sf::gdal_utils(
                    util = "warp"
                    , source = gdalSDS
                    , destination = ofile
                    , options = c(
                      params
                      , "-overwrite"
                      , if (qt) {
                        "-multi"
                      }
                    )
                    , quiet = qt
                  )
                  
                  # ## if required, remove temporary file
                  # if (file.exists(dmy)) {
                  #   jnk = file.remove(dmy)
                  # }
                }
                
                ofiles[i] <- ofile
              }
              
              lst_ofile[[l]] <- ofiles
            } else {
              warning(paste0("No file found for date: ",avDates[l]))
              lst_ofile[[l]] <- NA
            }
          }
          
          names(lst_ofile) <- avDates
          lst_todo[[u]] <- lst_ofile
          
        } else {
          warning(paste("No", product@PRODUCT, "files found for the period from"
                        , tLimits$begin, "to", paste0(tLimits$end, ".")))
          lst_todo[[u]] <- NA
        }
      }
      
      lst_product[[z]] <- lst_todo[[1]]
    }
    
    names(lst_product) <- paste(product@PRODUCT, product@CCC, sep = ".")
    return(lst_product)
}

