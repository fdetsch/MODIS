#' Process MODIS HDF with GDAL
#' 
#' @description 
#' Downloads MODIS grid data from archive (HTTP or local) and processes the 
#' files.
#' 
#' @param product \code{character}, see \code{\link{getProduct}}.
#' @param collection \code{character} or \code{integer}, see 
#' \code{\link{getCollection}}.
#' @param begin,end \code{Date} or \code{character}. Begin and end date of MODIS 
#' time series, see \code{\link{transDate}}.
#' @param extent Extent information, defaults to \code{'global'}. See
#' \code{\link{getTile}}.
#' @param tileH,tileV \code{numeric} or \code{character}. Horizontal and 
#' vertical tile number, see \code{\link{getTile}}.
#' @param SDSstring \code{character}, see \code{\link{getSds}}.
#' @param job \code{character}. Name of the current job for the creation of the 
#' output folder. If not specified, it is created in 'PRODUCT.COLLECTION_DATETIME'.
#' @param checkIntegrity \code{logical}, see \code{\link{getHdf}}. 
#' @param forceDownload \code{logical}, see \code{\link{getHdf}}.
#' @param overwrite \code{logical}, defaults to \code{FALSE}. Determines 
#' whether or not to overwrite existing SDS output files. 
#' @param maskValue Currently ignored. 
#' @param ... Additional arguments passed to \code{\link{MODISoptions}}, e.g. 
#' 'wait'. Permanent settings for these arguments are temporarily overridden.
#' 
#' @return 
#' A \code{list} of the same length as 'product'. Each product slot either holds 
#' a sub-\code{list} of processed dates which, for each time step, include the 
#' corresponding output files as \code{character} objects or, if no files could 
#' be found for the specified time period, a single \code{NA}.
#' 
#' @details 
#' \describe{
#' \tabular{rll}{
#'   \tab \code{outProj}\tab CRS/ prj4 or EPSG code of output, any format supported by gdal see examples.\cr \tab \tab Default is 'asIn' (no warping). See \code{?MODISoptions}.\cr
#'   \tab \code{pixelSize}\tab Numeric single value. Output pixel size in target reference system unit.\cr \tab \tab Default is 'asIn'. See \code{?MODISoptions}.\cr
#'   \tab \code{resamplingType}\tab Character. Default is 'near', can be one of: 'bilinear', 'cubic', 'cubicspline', 'lanczos'.\cr \tab \tab See \code{?MODISoptions}.\cr
#'   \tab \code{blockSize}\tab integer. Default \code{NULL} that means the stripe size is set by GDAL.\cr \tab \tab Basically it is the "-co BLOCKYSIZE=" parameter. See: http://www.gdal.org/frmt_gtiff.html\cr
#'   \tab \code{compression}\tab logical. Default is \code{TRUE}, compress data with the lossless LZW compression with "predictor=2".\cr \tab \tab See: \url{http://www.gdal.org/frmt_gtiff.html}\cr
#'   \tab \code{dataFormat}\tab Data output format, see \code{getOption("MODIS_gdalOutDriver")} column 'name'.\cr
#'   \tab \code{localArcPath}\tab Character.  See \code{?MODISoptions}. Local path to look for and/or to download MODIS files.\cr
#'   \tab \code{outDirPath}\tab Character.  See \code{?MODISoptions}. Root directory where to write \code{job} folder.\cr
#' }
#' }
#' 
#' \code{\link{runGdal}} uses numerous \strong{MODIS} functions under the hood, 
#' see the linked functions in 'Arguments' for details and inputs.\cr
#' 
#' If \code{extent} is a \code{Raster*} object, the output has exactly the same 
#' extent, pixel size, and projection.\cr
#' If \code{extent} is a \strong{sp} or \strong{sf} object, the 
#' output has exactly the same extent and projection except for point geometries 
#' with \emph{length = 1} (ie. a single point) where only the projection is 
#' inherited.\cr
#' If \code{tileH} and \code{tileV} are used (instead of \code{extent}) to 
#' define the area of interest, and \code{outProj} and \code{pixelSize} are 
#' \code{'asIn'}, the result is only converted from multilayer-HDF to 
#' \code{dataFormat}, default "GeoTiff" (\code{\link{MODISoptions}}).\cr
#' 
#' @author 
#' Matteo Mattiuzzi, Florian Detsch
#' 
#' @seealso 
#' \code{\link{getHdf}}, \code{\link{runMrt}}.
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
    ## http://www.gdal.org/frmt_gtiff.html)
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
        if (length(server) > 1) {
          server = server[which(server == opts$MODISserverOrder[1])]
        } 
          
        ftpdirs[[1]] <- as.Date(getStruc(product = strsplit(todo[u], "\\.")[[1]][1],
                                         collection = strsplit(todo[u], "\\.")[[1]][2],
                                         begin = tLimits$begin, end = tLimits$end,
                                         server = server)$dates)
        
        prodname <- strsplit(todo[u],"\\.")[[1]][1] 
        coll     <- strsplit(todo[u],"\\.")[[1]][2]
        
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
            
            if(length(files)>0)
            {
              SDS = lapply(
                files
                , function(y) {
                  getSds(
                    HdfName = y
                    , SDSstring = SDSstring
                  )
                }
              )

              NAS <- getNa(SDS[[1]]$SDS4gdal)

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
                nodataValue = srcnodata = dstnodata = if (length(naID) > 0) NAS[[naID]]

                if (!file.exists(ofile) || overwrite) {
                  
                  if (file.exists(ofile)) {
                    jnk = file.remove(ofile)
                  }
                  
                  ## create first set of gdal options required by subsequent step
                  lst = list(dataFormat, co, rt, srcnodata, dstnodata)
                  names(lst) = paste0(
                    "-"
                    , c("of", "co", "r", "srcnodata", "dstnodata")
                  )
                  lst = Filter(Negate(is.null), lst)
                  
                  params = character()
                  for (j in seq(lst)) {
                    params = c(params, names(lst)[j], lst[[j]])
                  }
                  
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
                        , "-multi"
                      )
                      , quiet = !is.null(opts$quiet) && opts$quiet
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
                  
                  
                  # ### 'maskValue' ----
                  # 
                  # ## dummy file
                  # dmy = tempfile("val_repl", tmpdir = normalizePath(raster::tmpDir()), fileext = xtn)
                  # 
                  # if (!is.null(maskValue)) {
                  #   
                  #   # check numeric
                  #   if (is.character(maskValue)) {
                  #     maskValue = as.numeric(maskValue)
                  #   } else if (!is.numeric(maskValue)) {
                  #     stop("'maskValue' needs to be numeric.")
                  #   }
                  #   
                  #   # if required, remove No Data Value from 'maskValue'
                  #   if (any(maskValue == nodataValue)) {
                  #     maskValue = maskValue[maskValue != nodataValue]
                  #   }
                  #   
                  #   ifile <- paste0(gdalSDS,collapse='" "')
                  #
                  #   # if No Data Value is not already defined 
                  #   if (is.null(srcnodata)) {
                  #     nodataValue = maskValue[1]
                  #     
                  #     if (length(maskValue) > 1) {
                  #       
                  #       for (w in 2:length(maskValue)) {
                  #         shell(paste(rpl
                  #                     , "-innd", maskValue[w]
                  #                     , "-outnd", nodataValue
                  #                     , of
                  #                     , ifelse(w == 2, ifile, ofile)
                  #                     , dmy))
                  #         
                  #         jnk = file.copy(dmy, ofile, overwrite = TRUE)
                  #       }
                  #     }
                  #     
                  #     srcnodata <- paste0(" -srcnodata ", nodataValue)
                  #     dstnodata <- paste0(" -dstnodata ", nodataValue)
                  #     
                  #   # if No Data Value is already defined  
                  #   } else {
                  #     
                  #     for (w in 1:length(maskValue)) {
                  #       shell(paste(rpl
                  #                   , "-innd", maskValue[w]
                  #                   , "-outnd", nodataValue
                  #                   , of
                  #                   , ifelse(w == 1, ifile, ofile)
                  #                   , dmy))
                  #       
                  #       jnk = file.copy(dmy, ofile, overwrite = TRUE)
                  #     }
                  #   }
                  #   
                  #   masked = file.exists(dmy)
                  # } else {
                  #   masked = FALSE
                  # }
                  
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
                      , "-multi"
                    )
                    , quiet = !is.null(opts$quiet) && opts$quiet
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

