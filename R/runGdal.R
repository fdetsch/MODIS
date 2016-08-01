#' Process MODIS hdf with GDAL
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
#' @param buffer \code{numeric} (in map units), see \code{\link{getTile}}.
#' @param SDSstring \code{character}, see \code{\link{getSds}}.
#' @param job \code{character}. Name of the current job for the creation of the 
#' output folder. If not specified, it is created in 'PRODUCT.COLLECTION_DATETIME'.
#' @param checkIntegrity \code{logical}. If \code{FALSE} (default), no file 
#' integrity check is performed, else the size of each downloaded file is 
#' checked. In case of inconsistencies, the function tries to re-download broken 
#' files.
#' @param wait \code{numeric}, see \code{\link{getHdf}}.
#' @param quiet \code{logical}, defaults to \code{TRUE}.
#' @param forceDownload \code{logical}, see \code{\link{getHdf}}.
#' @param ... See Methods.
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
#' Matteo Mattiuzzi
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
runGdal <- function(product, collection=NULL, begin=NULL,end=NULL, extent=NULL, tileH=NULL, tileV=NULL, buffer=0, SDSstring=NULL, job=NULL, checkIntegrity=TRUE, wait=0.5, quiet=FALSE,forceDownload=TRUE,...)
{
    opts <- combineOptions(...)
    # debug:
    # opts    <- combineOptions();product="MYD09GQ";collection=NULL; begin='2007.12.03'; end='2007.12.03'; extent=siteExtent; tileH=NULL; tileV=NULL; buffer=0; SDSstring=NULL; job=NULL; checkIntegrity=TRUE; wait=0.5; quiet=FALSE
          
    if(!opts$gdalOk)
    {
        stop("GDAL not installed or configured, read in '?MODISoptions' for help")
    }
    # absolutly needed
    product <- getProduct(product,quiet=TRUE)
    
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
        extent <- getTile(extent=extent, tileH=tileH, tileV=tileV, buffer=buffer)
    } else
    {
        extent <- NULL
    }

    #### outProj
    t_srs <- NULL
    cat("########################\n")
    if(!is.null(extent$target$outProj))
    {
      outProj <- checkOutProj(extent$target$outProj,tool="GDAL")
      cat("outProj          = ",outProj ," (Specified by raster*/spatial* object)\n")
    } else
    {
      outProj <- checkOutProj(opts$outProj,tool="GDAL")
      cat("outProj          = ",outProj,"\n")
    }
    if (outProj == "asIn")
    {
        if (product$SENSOR[1]=="MODIS")
        {
            if (product$TYPE[1]=="Tile")
            {
                outProj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
            } else 
            {
                outProj <- "+proj=longlat +ellps=clrk66 +no_defs" # CMG proj
            }
        } else if (product$SENSOR[1]=="SRTM")
        {
            outProj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
        } 
    }
    t_srs <- paste0(' -t_srs ',shQuote(outProj))
    
    #### pixelSize
    if(!is.null(extent$target$pixelSize))
    {
      pixelSize <- extent$target$pixelSize
      cat("pixelSize        = ",pixelSize ," (Specified by raster* object)\n")
    } else 
    {
      pixelSize <- opts$pixelSize
      cat("pixelSize        = ",pixelSize,"\n")
    } 

    tr <- NULL
    if (pixelSize[1]!="asIn")
    {
      if (length(pixelSize)==1)
      {
        tr <- paste(" -tr",pixelSize,pixelSize)
      } else
      {
        tr <- paste0(" -tr ", paste0(pixelSize,collapse=" "))
      }
    }
    
    #### resamplingType
    opts$resamplingType <- checkResamplingType(opts$resamplingType, tool="gdal")
    cat("resamplingType   = ", opts$resamplingType,"\n")
    rt <- paste0(" -r ",opts$resamplingType)
    
    #### inProj (s_srs)    
    if (product$SENSOR[1]=="MODIS")
    {
      if (product$TYPE[1]=="Tile")
      {
        s_srs <- paste0(' -s_srs ',shQuote("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
      } else 
      {
        s_srs <- paste0(' -s_srs ',shQuote("+proj=longlat +ellps=clrk66 +no_defs"))
      }
    } else if (product$SENSOR[1]=="SRTM")
    {
      s_srs <- paste0(' -s_srs ',shQuote("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    }
    #### te (target @extent)
    te <- NULL # if extent comes from tileV/H
    if (!is.null(extent$target$extent)) # all extents but not tileV/H
    {
      if (is.null(extent$target$outProj)) # map or list extents (always LatLon)
      {
        rx <- raster(extent$target$extent,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 
        rx <- projectExtent(rx,outProj)
        rx <- extent(rx) 
      } else
      {
        rx <- extent$target$extent
      }
      te <- paste(" -te", rx@xmin, rx@ymin, rx@xmax, rx@ymax)  
    } 
    if (is.null(extent$target))
    {
      if(!is.null(extent$extent))
      {
        rx <- raster(extent$extent,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 
        rx <- projectExtent(rx,outProj)
        rx <- extent(rx) 
        te <- paste(" -te", rx@xmin, rx@ymin, rx@xmax, rx@ymax)  
      }
    }
    
    #### generate non-obligatory GDAL arguments
    # GeoTiff BLOCKYSIZE and compression. See: http://www.gdal.org/frmt_gtiff.html            
    if(is.null(opts$blockSize))
    {
      bs <- NULL
    } else
    {
      opts$blockSize <- as.integer(opts$blockSize)
      bs <- paste0(" -co BLOCKYSIZE=",opts$blockSize)
    }
      
    # compress output data
    if(is.null(opts$compression))
    {
      cp <- " -co compress=lzw -co predictor=2"
    } else if (isTRUE(opts$compression))
    {
      cp <- " -co compress=lzw -co predictor=2"
    } else
    {
      cp <- NULL
    }
    ####
    if (quiet)
    {
      q <- " -q"
    } else
    {
      q <- NULL
    }
    
    for (z in seq_along(product$PRODUCT))
    { # z=1
      todo <- paste(product$PRODUCT[z],".",product$CCC[[product$PRODUCT[z]]],sep="")    
      
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
        dir.create(outDir,showWarnings=FALSE,recursive=TRUE)
      }
      
      for(u in seq_along(todo))
      { # u=1
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
                      
          for (l in seq_along(avDates))
          { # l=1
            files <- unlist(
              getHdf(product=prodname, collection=coll, begin=avDates[l], end=avDates[l],
               tileH=extent$tileH, tileV=extent$tileV, checkIntegrity=checkIntegrity, 
               stubbornness=opts$stubbornness, MODISserverOrder=opts$MODISserverOrder, 
               forceDownload = forceDownload)
            )
            
            files <- files[basename(files)!="NA"] # is not a true NA so it need to be like that na not !is.na()
            
            if(length(files)>0)
            {
              w <- getOption("warn")
              options("warn"= -1)
              SDS <- list()
              for (z in seq_along(files))
              { # get all SDS names for one chunk
                SDS[[z]] <- getSds(HdfName=files[z], SDSstring=SDSstring, method="GDAL")
              }
              options("warn"= w)
            
              if (!exists("NAS"))
              {
                NAS <- getNa(SDS[[1]]$SDS4gdal)
              }
               
              for (i in seq_along(SDS[[1]]$SDSnames))
              { # i=1
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
                if (.Platform$OS=="unix")
                {
                  ifile <- paste0(gdalSDS,collapse="' '")
                  ofile <- paste0(outDir, '/', outname)
                  cmd   <- paste0(opts$gdalPath,
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
                            ofile
                            )
                  cmd <- gsub(x=cmd,pattern="\"",replacement="'")
                  system(cmd)
                } else # windows
                {
                  cmd <- paste0(opts$gdalPath,"gdalwarp")
               
                  # ifile <- paste(shortPathName(gdalSDS),collapse='\" \"',sep=' ')
                  # ofile <- shortPathName(paste0(normalizePath(outDir), '\\', outname))
                  ofile <- paste0(outDir, '/', outname)      
                  ifile <- paste0(gdalSDS,collapse='" "')
                  
                  # GDAL < 1.8.0 doesn't support ' -overwrite' 
                  if(file.exists(ofile))
                  {
                    invisible(file.remove(ofile))
                  }
                    shell(
                       paste(cmd,
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
                        ' \"', ofile,'\"',
                       sep = '')
                      ) 
                   }
                    if(length(grep(todo,pattern="M.D13C2\\.005"))>0)
                    {
                      unlink(list.files(path=outDir,pattern=ranpat,full.names=TRUE),recursive=TRUE)
                    }
                  }
                } else
                {
                  warning(paste0("No file found for date: ",avDates[l]))
                }
               }
            }
        }
    }
}

