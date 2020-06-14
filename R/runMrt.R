#' Run MODIS Reprojection Tool
#' 
#' @description 
#' Specifying input parameters, this function gets MODIS grid data from the 
#' archive (HTTP or local) and processes it with the MODIS Reprojection Tool 
#' (MRT). At any point, you are highly encouraged to consult the
#' MRT User's Manual (see References) for further information.
#' 
#' @param product,collection,begin,end,extent,tileH,tileV,SDSstring,job See \code{\link[MODIS]{runGdal}}
#' and functions linked therein.
#' @param datum The output datum used for datum conversion as \code{character}, 
#' defaults to \code{"NODATUM"}. Supported datums are \code{"NAD27"}, 
#' \code{"NAD83"}, \code{"WGS66"}, \code{"WGS72"} and \code{"WGS84"}, see 
#' MRT User's Manual, p. 7-8.
#' @param zone Output zone number as \code{integer}, relevant only for 
#' UTM projections (i.e., \code{outProj = "UTM"}. Valid values are \code{–60} to 
#' \code{+60}. 
#' @param projPara Output projection parameters as \code{character} string, see 
#' 'Details'. Ignored if \code{outProj \%in\% c("SIN", "GEO")}. If not specified 
#' and using another target projection, the default settings for \code{"GEO"} 
#' are assumed.
#' @param mosaic A \code{logical} that toggles mosaicking on (default) or off. 
#' One example where \code{mosaic = FALSE} makes sense is for large spatial 
#' extents because maximum supported HDF4 filesize is 2GB; if crossed, 
#' mosaicking will fail.
#' @param anonym A \code{logical}, defaults to \code{TRUE}. If \code{FALSE}, the 
#' job name is appended to the root filename.
#' @param ... Additional arguments passed to \code{\link[MODIS]{MODISoptions}}, 
#' see also 'Details' for some MRT specific settings.
#' 
#' @return 
#' A \code{list} of output filenames summarized by product and date, see also 
#' Value in \code{link[MODIS]{runGdal}}.
#' 
#' @details 
#' Please note that in contrast to \code{\link[MODIS]{runGdal}}, MRT's 
#' \code{resample} function does not offer an 'overwrite' option, and hence, 
#' existing files will be overwritten (see also MRT User's Manual, p. 59).
#' Further arguments that require particular attention when operating MRT are 
#' summarized in the following list:
#'  
#' \strong{\code{dataFormat}}:\cr\cr
#' Output file formats include:
#' 
#' \itemize{
#' \item{\code{"raw binary"} (\code{.hdr} and \code{.dat})}
#' \item{\code{"HDF-EOS"} (\code{.hdf})}
#' \item{\code{"GeoTiff"} (\code{.tif}; default)}
#' }
#' 
#' Any other format specified through \code{\link[MODIS]{MODISoptions}} or 
#' 'dataFormat' is ignored and set to \code{"GeoTiff"}.
#' 
#' \strong{\code{outProj}}:\cr\cr
#' MRT uses calls to the General Cartographic Transformation Package (GCTP) and as such allows projection to the following mapping 
#' grids:
#' 
#' \itemize{
#' \item{Albers Equal Area (\code{"AEA"})}
#' \item{Equirectangular (\code{"ER"})}
#' \item{Geographic (\code{"GEO"})}
#' \item{Hammer (\code{"HAM"})}
#' \item{Integerized Sinusoidal (\code{"ISIN"})}
#' \item{Interrupted Goode Homolosine (\code{"IGH"})}
#' \item{Lambert Azimuthal (\code{"LA"})}
#' \item{Lambert Conformal Conic (\code{"LCC"})}
#' \item{Mercator (\code{"MERCAT"})}
#' \item{Molleweide (\code{"MOL"})}
#' \item{Polar Stereographic (\code{"PS"})}
#' \item{Sinusoidal (\code{"SIN"})}
#' \item{Transverse Mercator (\code{"TM"})}
#' \item{Universal Transverse Mercator (\code{"UTM"})}
#' }
#' 
#' See also 'References' and MRT User's Manual, pp. 6 and 29.
#' 
#' \strong{\code{projPara}}:\cr\cr
#' Output projection parameters are autodetected for 
#' \code{outProj \%in\% c("SIN", "GEO")}:
#' 
#' \itemize{
#' \item{\code{"SIN"}: \code{"6371007.18 0.00 0.00 0.00 0.00 0.00 0.00 0.00 86400.00 0.00 0.00 0.00 0.00 0.00 0.00"}}
#' \item{\code{"GEO"}: \code{"0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0"}}
#' }
#' 
#' For detailed information on defining paramters for other target projections, 
#' please refer to 'Appendix C: Projection Parameters' in the MRT User's Manual, 
#' p. 65-66. 
#' 
#' @author 
#' Matteo Mattiuzzi, Forrest Stevens and Florian Detsch
#' 
#' @seealso 
#' \code{\link[MODIS]{MODISoptions}}, \code{\link[MODIS]{runGdal}}.
#' 
#' @source 
#' The MRT software has been \href{https://lpdaac.usgs.gov/news/downloadable-modis-reprojection-tool-mrt-and-mrtswath-tool-have-been-retired}{retired}, 
#' and is hence no longer officialy available for download through LP DAAC.
#' 
#' @references 
#' Dwyer J, Schmidt G (2006) The MODIS Reprojection Tool, 162-177,
#' doi:\href{https://doi.org/10.1007/978-3-540-37294-3_9}{10.1007/978-3-540-37294-3_9}. 
#' In: Qu JJ, Gao W, Kafatos M, Murphy RE, Salomonson VV (eds) Earth Science 
#' Satellite Remote Sensing. Springer: Berlin, Heidelberg.
#' 
#' Elassal AA (1989) General Cartographic Transformation Package (GCTP), Version 
#' II. NOAA Technical Report NOS124 CGS9. NOAA: Rockville, MD, USA. Available 
#' online \href{https://www.cmascenter.org/ioapi/documentation/all_versions/html/GCTP.pdf}{here}
#' (2018-09-13).
#' 
#' Land Processes DAAC, USGS Earth Resources Observation and Science Center 
#' (2011) MODIS Reprojection Tool User's Manual. Release 4.1, April 2011. 
#' Available online \href{http://www2.fct.unesp.br/docentes/carto/enner/Processamento\%20MODIS/MRT_usermanual.pdf}{here}.
#' 
#' @examples 
#' \dontrun{
#' geo = runMrt(product="MOD11A1", extent="austria", begin="2010001", end="2010002", SDSstring="101",
#'              job="ExampleGEOdelme", outProj="GEO")
#' sin = runMrt(product="MOD11A1", extent="austria", begin="2010001", end="2010002", SDSstring="101",
#'              job="ExampleSINdelme", outProj="SIN")
#' utm = runMrt(product="MOD11A1", extent="austria", begin="2010001", end="2010002", SDSstring="101",
#'              job="ExampleUTMdelme", outProj="UTM", zone = 33)
#' }
#' 
#' @export runMrt
#' @name runMrt
runMrt <- function(product, collection = NULL
                   , begin = NULL, end = NULL
                   , extent = NULL, tileH = NULL, tileV = NULL
                   , SDSstring = NULL, job = NULL
                   , datum = "NODATUM", zone = NULL, projPara = NULL
                   , mosaic = TRUE, anonym = TRUE
                   , ...)
{

    dots = list(...)
    opts = do.call(combineOptions, dots)

    if (!opts$mrtOk)
    {
        stop("MRT path not set or MRT not installed on your system!")
    }
        
    product     <- getProduct(product, quiet = TRUE, collection = collection)
    tLimits          <- transDate(begin=begin,end=end)
    
    opts$localArcPath <- setPath(opts$localArcPath)
    opts$outDirPath   <- setPath(opts$outDirPath)
    
    if(!tolower(opts$dataFormat) %in% c('raw binary', 'hdf-eos', 'hdf4image','gtiff', 'geotiff'))
    {
        stop('dataFormat=\'',opts$dataFormat ,'\' is not supported by MRT (only \'raw binary\', \'HDF-EOS\' or \'GeoTiff\')')
    } 
    ext = checkMrtWriteDriver(opts$dataFormat)
    
    if (!inherits(extent, "MODISextent")) {
      extent = if (product@TYPE[1] == "Tile" |
                   (all(!is.null(extent) | !is.null(tileH) & !is.null(tileV)) &
                    product@TYPE[1]=="CMG")) {
        getTile(x = extent, tileH = tileH, tileV = tileV
                , outProj = "asIn"
                , pixelSize = "asIn")
      } else {
        NULL
      }
    }
    
    opts$resamplingType <- checkResamplingType(opts$resamplingType,tool="mrt",quiet=TRUE)
    opts$outProj        <- checkOutProj(opts$outProj,tool="mrt",quiet=TRUE)
    
    if (opts$outProj[1]=="asIn")
    {
      opts$outProj <- list(short="SIN",long="Sinusoidal")
    }
    cat("Output projection:", opts$outProj$long,"\n")    
    if (opts$outProj$short=="UTM")
    {
      zone = checkUTMZone(zone)
      
        if (is.null(zone)) 
        {
            cat("No UTM zone specified using MRT autodetection.\n")            
        } else 
        {
            cat("Using UTM zone:", zone,"\n")
        }
    }

    cat("Output pixel size:", opts$pixelSize,"\n")
    cat("Resampling method:", opts$resamplingType,"\n")
 
    if (!toupper(datum) %in% c("NAD27", "NAD83", "WGS66", "WGS72", "WGS84", "NODATUM"))
    {
      stop('Output "datum" must be one of: "NAD27", "NAD83", "WGS66", "WGS72", "WGS84" or "NODATUM"')
    } else {
      cat("Datum conversion:", datum, "\n")
    }
    
    if (is.null(projPara)) 
    {
        if(opts$outProj$short=="SIN") # maybe we should add other
        {
            projPara <- "6371007.18 0.00 0.00 0.00 0.00 0.00 0.00 0.00 86400.00 0.00 0.00 0.00 0.00 0.00 0.00"
        } else 
        {
            cat("No output projection parameters specified, using 'GEO' default settings!\n")
            projPara <- "0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0"
        }
    } else 
    {
        cat("Output projection parameters specified!\nUsing:\n ",projPara,"\n")
    }

    lst_product <- vector("list", length(product@PRODUCT))
    for (z in 1:length(product@PRODUCT))
    {
      
      # if (product@TYPE[z]=="CMG") 
      # {
      #   tileID="GLOBAL"
      #   ntiles=1 
      # } else 
      # {
      #   extent <- getTile(x=extent,tileH=tileH,tileV=tileV)
      #   ntiles    <- length(extent@tile)
      # }

        todo <- paste(product@PRODUCT[z],".",product@CCC[[product@PRODUCT[z]]],sep="")    
    
        lst_todo <- vector("list", length(todo))
        for(u in 1:length(todo))
        {
            if (is.null(job))
            {
                job <- paste(todo[u],"_",format(Sys.time(), "%Y%m%d%H%M%S"),sep="")    
                cat("No 'job' name specified, generated (date/time based)):",job,"\n")
            }
            outDir <- gsub("//", "/", file.path(opts$outDirPath,job,fsep="/"))
            dir.create(outDir, showWarnings = FALSE)

            ######################## along platform (TerraAqua)
            ftpdirs <- list()    ##  FRS: Fix provided by Ahmadou Dicko
            ftpdirs[[1]] <- as.Date(getStruc(product=strsplit(todo[u],"\\.")[[1]][1],collection=strsplit(todo[u],"\\.")[[1]][2],begin=tLimits$begin,end=tLimits$end,server=opts$MODISserverOrder[1])$dates)
            
            prodname <- strsplit(todo[u],"\\.")[[1]][1] 
            coll     <- strsplit(todo[u],"\\.")[[1]][2]
            
            avDates <- ftpdirs[[1]]
            avDates <- avDates[avDates!=FALSE]
            avDates <- avDates[!is.na(avDates)]        
            
            sel     <- as.Date(avDates)
            
            st = correctStartDate(tLimits$begin, sel, prodname, quiet = opts$quiet)
            us      <- sel >= st & sel <= tLimits$end
            
            if (sum(us,na.rm=TRUE)>0)
            {
                avDates <- avDates[us]

            ######################### along begin -> end date
                lst_ofile <- as.list(rep(NA, length(avDates)))
                for (l in 1:length(avDates))
                { 
                  files = unlist(
                    do.call(getHdf, append(list(product = product@PRODUCT[z]
                                                , collection = strsplit(todo[u], "\\.")[[1]][2]
                                                , begin = avDates[l], end = avDates[l]
                                                , tileH = extent@tileH, tileV = extent@tileV)
                                      , opts))
                  )

                    if (length(files)!=0)
                    {
                        mos <- mosaic
        
                        if (mos)
                        {
                            # if not all files available switch "off" mosaicking and process single files. Problematic in areas with tiles outside land!
                            if (sum(file.exists(files)) < length(extent@tile))
                            {
                                mos <- FALSE
                            } else {
                                mos <- TRUE
                            }
            
                        } else { 
                            mos <-  FALSE
                        }
            
                        if (mos)
                        {
                            v <- 1
                        } else {
                            v <- seq_along(files)
                        }
            
                        for (q in v)
                        {
                            w <- options("warn")
                            options(warn=-1)
                            if (is.null(SDSstring))
                            {
                                SDSstring <- rep(1,length(getSds(HdfName=files[q],method="mrt")$SDSnames))
                            }    
                
                            SDSstringIntern <- getSds(HdfName=files[q],SDSstring=SDSstring,method="mrt")
                            options(warn=w$warn)
                            
                            if (!opts$quiet && u == 1 && l == 1)
                            {
                                cat("\n#############################\nExtracing SDS:",SDSstringIntern$SDSnames,"#############################\n",sep="\n")
                            }
                            
                            if (mos)
                            {
                                TmpMosNam <- paste("TmpMosaic",makeRandomString(),".hdf",sep="")
                                ### in subset
                                paraname <- file.path(outDir,"/MRTgMosaic.prm",fsep="/") # create mosaic prm file
                                on.exit({
                                  try(unlink(paraname), silent = TRUE)
                                  try(unlink(paste(outDir,TmpMosNam,sep="/")), silent = TRUE)
                                })
                                
                                filename = file(paraname, open="wt")
                                on.exit(try(close(filename), silent = TRUE))
                                
                                write(paste("\"",files,"\"",sep='',collapse=' '), filename)
                                close(filename)
                        
                                # run mosaic
                                if (.Platform$OS=="unix")
                                {
                                    system(paste("mrtmosaic -i ",paraname," -o ",outDir,"/",TmpMosNam," -s '",SDSstringIntern$SDSstring,"'" ,sep=""))
                                } else 
                                {
                                    shell(paste("mrtmosaic -i \"",paraname,"\" -o \"", normalizePath(outDir) ,"\\",TmpMosNam,"\" -s \"",SDSstringIntern$SDSstring,"\"" ,sep=""))
                                }
                                unlink(paraname)
                                Sys.sleep(opts$wait) # without wait the skript can break here. "wait" is a try but it seams to work!!!
                            }
            
                            basenam <- strsplit(files[q],"/")[[1]]
                            basenam <- basenam[length(basenam)]
        
                            if (mos)
                            {
                                basenam <- paste(strsplit(basenam,"\\.")[[1]][c(1,2,4)],collapse=".")
                            } else {
                                basenam <- paste(strsplit(basenam,"\\.")[[1]][c(1,2,3,4)],collapse=".")    
                            }
        
                            if (!anonym)
                            {   
                                basenam <- paste(basenam,job,sep=".")
                            }
    
                            #### Write prm File
                            paraname <- paste(outDir,"/MRTgResample.prm",sep="")
                            on.exit({
                              try(unlink(paraname), silent = TRUE)
                            })
                            
                            filename = file(paraname, open="wt")
                            on.exit(try(close(filename), silent = TRUE))

                            if (mos)
                            {
                                write(paste('INPUT_FILENAME = "',outDir,"/",TmpMosNam,'"',sep=''), filename)
                            } else 
                            {
                                write(paste('SPECTRAL_SUBSET = ( ',SDSstringIntern$SDSstring,' )',sep=''), filename)
                                write(paste('INPUT_FILENAME = "',files[q],'"',sep=''), filename)
                            }
    
                            write('SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG',filename)
    
                            if (!is.null(extent@extent))
                            {
                                write(paste('SPATIAL_SUBSET_UL_CORNER = (',extent@extent@ymax,' ',extent@extent@xmin,')',sep=''),filename)
                                write(paste('SPATIAL_SUBSET_LR_CORNER = (',extent@extent@ymin,' ',extent@extent@xmax,')',sep=''),filename)
                            }
                            if (opts$pixelSize!="asIn")
                            {
                                write(paste('OUTPUT_PIXEL_SIZE = ',opts$pixelSize,sep=''),filename) 
                            }    
                            write(paste('OUTPUT_FILENAME = ',outDir,"/",basenam,ext,sep=''),filename) 
                            write(paste('RESAMPLING_TYPE = ',opts$resamplingType,sep=''),filename)
                            
                            write(paste('OUTPUT_PROJECTION_TYPE = ',opts$outProj$short,sep=''),filename)
                            
                            if (opts$outProj$short=="UTM" && !is.null(zone))
                            {
                                write(paste('UTM_ZONE = ',zone,sep=''),filename)
                            }
        
                            if (!is.null(projPara))
                            {
                                write(paste('OUTPUT_PROJECTION_PARAMETERS = ( ',projPara,' )',sep=''),filename)
                            }
                            if (!is.null(datum))
                            {
                                write(paste('DATUM = ', datum,sep=''),filename)
                            }
                            close(filename)
    
                            if (.Platform$OS=="unix")
                            {
                                system(paste("resample -p ",paraname,sep=""))
                            } else {
                                shell(paste("resample -p \"",paraname,"\" -o \"",outDir,"/",basenam, ext,sep=""))
                            }
                            unlink(paraname)
    
                            if (mos)
                            {
                                unlink(paste(outDir,TmpMosNam,sep="/"))
                            }
                        }
                      
                      xtr = gsub(" ", "_", SDSstringIntern$SDSnames)
                      lst_ofile[[l]] = sapply(xtr, function(pttrn) {
                        list.files(outDir, full.names = TRUE, ignore.case = TRUE
                                   , pattern = paste(basenam, pttrn, ext, sep = ".*"))
                      })
                      lst_ofile[[l]] = as.character(lst_ofile[[l]])
                      
                    } else {
                      warning(paste0("No file found for date: ",avDates[l]))
                      lst_ofile[[l]] <- NA
                    }
                } # l, avDates
                
              names(lst_ofile) <- avDates
              lst_todo[[u]] <- lst_ofile
                
            } else {
              warning(paste("No", product@PRODUCT, "files found for the period from"
                            , tLimits$begin, "to", paste0(tLimits$end, ".")))
              lst_todo[[u]] <- NA
            }
        } # u   
        
        lst_product[[z]] <- lst_todo[[1]]
    } # z

    names(lst_product) <- paste(product@PRODUCT, product@CCC, sep = ".")
    return(lst_product)
}

