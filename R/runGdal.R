#' Process MODIS HDF with GDAL
#' 
#' @description 
#' Downloads MODIS grid data from archive (FTP or local) and processes the 
#' files.
#' 
#' @param product \code{character}, see \code{\link{getProduct}}.
#' @param collection \code{character} or \code{integer}, see 
#' \code{\link{getCollection}}.
#' @param begin \code{character}. Begin date of MODIS time series, see 
#' \code{\link{transDate}} for formatting.
#' @param end Same for end date.
#' @param extent Extent information, defaults to \code{'global'}. See
#' \code{\link{getTile}}.
#' @param tileH \code{numeric} or \code{character}. Horizontal tile number, 
#' see \code{\link{getTile}}.
#' @param tileV \code{numeric} or \code{character}. Vertical tile number(s), 
#' see \code{tileH}.
#' @param SDSstring \code{character}, see \code{\link{getSds}}.
#' @param job \code{character}. Name of the current job for the creation of the 
#' output folder. If not specified, it is created in 'PRODUCT.COLLECTION_DATETIME'.
#' @param checkIntegrity \code{logical}, see \code{\link{getHdf}}. 
#' @param forceDownload \code{logical}, see \code{\link{getHdf}}.
#' @param overwrite \code{logical}, defaults to \code{FALSE}. Determines 
#' whether or not to overwrite existing SDS output files.
#' @param ... Additional arguments passed to \code{MODIS:::combineOptions()} (eg
#' 'wait'), see also \code{\link{MODISoptions}}.
#' 
#' @return 
#' A \code{list} of the same length as 'product'. Each product slot holds a 
#' sub-\code{list} of processed dates which, for each time step, include the 
#' corresponding output files as \code{character} objects. 
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
#' \code{\link{runGdal}} uses a lot of \strong{MODIS} package functions, see in 
#' section Arguments and Methods the respective '?function' for details and 
#' inputs.\cr
#' If \code{extent} is a \code{Raster*} object, the output has exactly the same 
#' extent, pixel size, and projection.\cr
#' If \code{extent} is a \strong{sp} object (i.e., polygon shapefile), the 
#' output has exactly the same extent and projection.\cr
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
#' @note 
#' You need to have a GDAL installed on your system!\cr
#' \url{http://www.gdal.org/gdal_utilities.html}\cr\cr
#' On Unix-alkes, install 'gdal-bin' (i.e. Ubuntu: 'sudo apt-get install gdal-bin')\cr
#' On Windows, you need to install GDAL through OSGeo4W 
#' (\url{http://trac.osgeo.org/osgeo4w/}) or FWTools 
#' (\url{http://fwtools.maptools.org/}) since the standard GDAL does not support 
#' HDF4 format.
#' 
#' @examples 
#' \dontrun{
#' # LST in Austria
#' runGdal( product="MOD11A1", extent="austria", begin="2010001", end="2010005", SDSstring="101")
#' 
#' # LST with interactiv area selection
#' runGdal( product="MOD11A1", begin="2010001", end="2010005", SDSstring="101")
#' 
#' ### outProj examples
#' # LST of Austria warped to UTM 34N (the three different possibilites to specify "outProj")
#' # to find am EPSG or prj4 you may use: prj <- make_EPSG() See  
#' runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
#'          SDSstring="101", outProj="EPSG:32634")
#' 
#' runGdal( job="LSTaustria", product="MOD11A1", extent="Austria", begin="2010001", end="2010005",
#'          SDSstring="101", outProj="32634")
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
                    extent=NULL, tileH=NULL, tileV=NULL, 
                    SDSstring=NULL, job=NULL, checkIntegrity=TRUE, 
                    forceDownload=TRUE, overwrite = FALSE, ...)
{
    opts <- combineOptions(...)

    if(!opts$gdalOk) {
        stop("GDAL not installed or configured, read in '?MODISoptions' for help")
    }
    
    # absolutely needed
    product <- getProduct(product, quiet=TRUE)
    
    # optional and if missing it is added here:
    product$CCC <- getCollection(product,collection=collection)
    tLimits     <- transDate(begin=begin,end=end)
    
    dataFormat <- toupper(opts$dataFormat) 
    if (dataFormat == 'RAW BINARY')
    {
        stop('in argument dataFormat=\'raw binary\', format not supported by GDAL (it is MRT specific) type: \'options("MODIS_gdalOutDriver")\' (column \'name\') to list available inputs')
    }
  
    if(dataFormat == 'HDF-EOS')
    {
        dataFormat <- "HDF4IMAGE"
    } else if(dataFormat == 'GEOTIFF')
    {
        dataFormat <- "GTIFF"
    }
    
    if(is.null(opts$gdalOutDriver))
    {
        opts$gdalOutDriver <- gdalWriteDriver()
        options("MODIS_gdalOutDriver"=opts$gdalOutDriver) # save for current session
    }
    
    if(dataFormat %in% toupper(opts$gdalOutDriver$name))
    {
        dataFormat <- grep(opts$gdalOutDriver$name, pattern=paste("^",dataFormat,"$",sep=""),ignore.case = TRUE,value=TRUE)
        of <- paste0(" -of ",dataFormat)
        extension  <- getExtension(dataFormat)
    } else 
    {
        stop('in argument dataFormat=\'',opts$dataFormat,'\', format not supported by GDAL type: \'gdalWriteDriver()\' (column \'name\') to list available inputs')
    }
    
    #### settings with messages
    # output pixel size in output proj units (default is "asIn", but there are 2 chances of changing this argument: pixelSize, and if extent comes from a Raster* object.
     
    if (product$TYPE[1]=="Tile" | (all(!is.null(extent) | !is.null(tileH) & !is.null(tileV)) & product$TYPE[1]=="CMG"))
    {
        extent <- getTile(x=extent, tileH=tileH, tileV=tileV)
    } else
    {
        extent <- NULL
    }

    
    ### GDAL command line arguments -----
    
    ## obligatory arguments
    t_srs <- OutProj(product, extent, opts) # outProj
    tr <- PixelSize(extent, opts)           # pixelSize
    rt <- ResamplingType(opts)              # resamplingType
    s_srs <- InProj(product)                # inProj
    te <- TargetExtent(extent,              # targetExtent
                       outProj = strsplit(t_srs, "'")[[1]][2]) 
    
    ## non-obligatory arguments (GTiff blocksize and compression, see 
    ## http://www.gdal.org/frmt_gtiff.html)
    bs <- BlockSize(opts)                   # blockSize
    cp <- OutputCompression(opts)           # compression
    q <- QuietOutput(opts)                  # quiet
    
    lst_product <- vector("list", length(product$PRODUCT))
    for (z in seq_along(product$PRODUCT)) {
      # z=1
      todo <- paste(product$PRODUCT[[z]], product$CCC[[product$PRODUCT[z]]], sep = ".")
      
      if(z==1)
      {
        if (is.null(job))
        {
          job <- paste0(todo[1],"_",format(Sys.time(), "%Y%m%d%H%M%S"))    
          cat("Output directory = ",paste0(normalizePath(opts$outDirPath,"/",mustWork=FALSE),"/",job)," (no 'job' name specified, generated (date/time based))\n")
        } else
        {
          cat("Output Directory = ",paste0(normalizePath(opts$outDirPath,"/",mustWork=FALSE),"/",job),"\n")
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
        
        if (length(unlist(product$SOURCE)) > 1) {
          server <- unlist(product$SOURCE)[which(unlist(product$SOURCE) == opts$MODISserverOrder[1])]
        } else {
          server <- unlist(product$SOURCE)
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
        us      <- sel >= tLimits$begin & sel <= tLimits$end
        
        if (sum(us,na.rm=TRUE)>0)
        {
          avDates <- avDates[us]
          
          lst_ofile <- vector("list", length(avDates))
          for (l in seq_along(avDates)) { 
            # l=1
            files <- unlist(
              getHdf(product=prodname, collection=coll, begin=avDates[l], end=avDates[l],
               tileH=extent$tileH, tileV=extent$tileV, checkIntegrity=checkIntegrity, 
               stubbornness=opts$stubbornness, MODISserverOrder=opts$MODISserverOrder, 
               forceDownload = forceDownload, wait = opts$wait)
            )
            
            files <- files[basename(files)!="NA"] # is not a true NA so it need to be like that na not !is.na()
            
            if(length(files)>0)
            {
              w <- getOption("warn")
              options("warn"= -1)
              SDS <- list()
              for (y in seq_along(files))
              { # get all SDS names for one chunk
                SDS[[y]] <- getSds(HdfName=files[y], SDSstring=SDSstring, method="GDAL")
              }
              options("warn"= w)
            
              if (!exists("NAS"))
              {
                NAS <- getNa(SDS[[1]]$SDS4gdal)
              }
               
              ## loop over sds
              ofiles <- character(length(SDS[[1]]$SDSnames))
              
              for (i in seq_along(SDS[[1]]$SDSnames)) {
                # i=1
                outname <- paste0(paste0(strsplit(basename(files[1]),"\\.")[[1]][1:2],collapse="."),
                   ".", gsub(SDS[[1]]$SDSnames[i],pattern=" ",replacement="_"), extension)
                  
                gdalSDS <- sapply(SDS,function(x){x$SDS4gdal[i]}) # get names of layer 'o' of all files (SDS)
                
                naID <- which(SDS[[1]]$SDSnames[i] == names(NAS))
                if(length(naID)>0)
                {
                  srcnodata <- paste0(" -srcnodata ",NAS[[naID]])
                  dstnodata <- paste0(" -dstnodata ",NAS[[naID]])
                } else
                {
                  srcnodata <- NULL
                  dstnodata <- NULL 
                }
 
                if(length(grep(todo,pattern="M.D13C2\\.005"))>0)
                {
                  if(i==1)
                  {
                    cat("\n###############\nM.D13C2.005 is likely to have a problem in metadata extent information, it is corrected on the fly\n###############\n") 
                  }
                  ranpat     <- makeRandomString(length=21)
                  randomName <- paste0(outDir,"/deleteMe_",ranpat,".tif") 
                  on.exit(unlink(list.files(path=outDir,pattern=ranpat,full.names=TRUE),recursive=TRUE))
                  for(ix in seq_along(gdalSDS))
                  {
                    cmd1 <- paste0(opts$gdalPath,"gdal_translate -a_nodata ",NAS[[naID]]," '",gdalSDS[ix],"' '",randomName[ix],"'")   
                    cmd2 <- paste0(opts$gdalPath,"gdal_edit.py -a_ullr -180 90 180 -90 '",randomName[ix],"'")
                    
                    if (.Platform$OS=="unix")
                    {
                      system(cmd1,intern=TRUE)
                      system(cmd2,intern=TRUE)
                    } else
                    {
                      shell(cmd1,intern=TRUE)
                      shell(cmd2,intern=TRUE)
                    }

                  }
                  gdalSDS <- randomName

                } 
                
                # unix
                if (.Platform$OS=="unix") {
                  
                  ifile <- paste0(gdalSDS,collapse="' '")
                  ofile <- paste0(outDir, '/', outname)
                  
                  if (!file.exists(ofile) | overwrite) {
                    cmd <- paste0(opts$gdalPath,
                                  "gdalwarp",
                                  s_srs,
                                  t_srs,
                                  of,
                                  te,
                                  tr,
                                  cp,
                                  bs,
                                  rt,
                                  q,
                                  srcnodata,
                                  dstnodata,
                                  " -overwrite",
                                  " -multi",
                                  " \'", ifile,"\'",
                                  " ",
                                  ofile)
                    
                    cmd <- gsub(x=cmd,pattern="\"",replacement="'")
                    system(cmd)
                  }
                  
                # windows  
                } else {
                  
                  cmd <- paste0(opts$gdalPath,"gdalwarp")
               
                  ofile <- paste0(outDir, '/', outname)      
                  ifile <- paste0(gdalSDS,collapse='" "')
                  
                  # GDAL < 1.8.0 doesn't support ' -overwrite' 
                  if (!file.exists(ofile) | overwrite) {
                    
                    if(file.exists(ofile))
                      invisible(file.remove(ofile))
                    
                    shell(
                      paste0(cmd,
                             s_srs,
                             t_srs,
                             of,
                             te,
                             tr,
                             cp,
                             bs,
                             rt,
                             q,
                             srcnodata,
                             dstnodata,
                             ' -multi',
                             ' \"', ifile,'\"',
                             ' \"', ofile,'\"')
                    )
                  }
                }
                
                if(length(grep(todo, pattern = "M.D13C2\\.005")) > 0) {
                  unlink(list.files(path = outDir, pattern = ranpat, 
                                    full.names = TRUE), recursive = TRUE)
                }
                
                ofiles[i] <- ofile
              }
              
              lst_ofile[[l]] <- ofiles
            } else {
              warning(paste0("No file found for date: ",avDates[l]))
              lst_ofile[[l]] <- NULL
            }
          }
          
          names(lst_ofile) <- avDates
          lst_todo[[u]] <- lst_ofile
          
        } else {
          lst_todo[[u]] <- NULL
        }
      }
      
      lst_product[[z]] <- lst_todo[[1]]
    }
    
    names(lst_product) <- paste(product$PRODUCT, product$CCC, sep = ".")
    return(lst_product)
}

