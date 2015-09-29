# Author: Matteo Mattiuzzi and Forrest Stevens
# Date : August 2011
# Licence GPL v3

runMrt <- function(...)
{
    MODISoptions(save=FALSE,quiet=TRUE)
    
    opts <- combineOptions(...)
    if (!opts$mrtOk)
    {
        stop("MRT path not set or MRT not installed on your system!")
    }
        
    opts$product     <- getProduct(opts$product,quiet=TRUE)
    opts$product$CCC <- getCollection(opts$product,collection=opts$collection)
    tLimits          <- transDate(begin=opts$begin,end=opts$end)
    
    opts$localArcPath <- setPath(opts$localArcPath)
    opts$outDirPath   <- setPath(opts$outDirPath)
    
    if(!tolower(opts$dataFormat) %in% c('raw binary', 'hdf-eos', 'hdf4image','gtiff', 'geotiff'))
    {
        stop('dataFormat=\'',opts$dataFormat ,'\' is not supported by MRT (only \'raw binary\', \'HDF-EOS\' or \'GeoTiff\')')
    } 
    ext <- getExtension(opts$dataFormat)
             
    ################################
    # Some defaults:
    if (is.null(opts$quiet))    {opts$quiet  <- FALSE} 
    if (is.null(opts$mosaic))   {opts$mosaic <- TRUE} 
    if (is.null(opts$anonym))   {opts$anonym <- TRUE} 

    opts$resamplingType <- checkResamplingType(opts$resamplingType,tool="mrt",quiet=TRUE)
    opts$outProj        <- checkOutProj(opts$outProj,tool="mrt",quiet=TRUE)
    
    if (opts$outProj[1]=="asIn")
    {
        if(as.numeric(opts$product$CCC) > 3) # this fails if a COLLECTION is "025"...don't know if this exists!
        {
            opts$outProj <- list(short="SIN",long="Sinusoidal")
        } else
        {
            opts$outProj <- list(short="ISIN", long="Integerized Sinusoidal")
        }
    }
    cat("Output projection:", opts$outProj$long,"\n")    
    if (opts$outProj$short=="UTM")
    {
        if (is.null(opts$zone)) 
        {
            cat("No UTM zone specified using MRT autodetection.\n")            
        } else 
        {
            cat("Using UTM zone:", opts$zone,"\n")
        }
    }

    cat("Output pixel size:", opts$pixelSize,"\n")
    cat("Resampling method:", opts$resamplingType,"\n")
 
    if (is.null(opts$datum))
    {
        cat("No Datum specified, using 'NODATUM'!\n")
        opts$datum <- "NODATUM"
    } else if (!toupper(opts$datum) %in% c("NAD27", "NAD83", "WGS66", "WGS72", "WGS84", "NODATUM"))
    {
        stop('"datum" must be one of: "NAD27", "NAD83", "WGS66", "WGS72", "WGS84" or "NODATUM"')
    }
    
    if (is.null(opts$projPara)) 
    {
        if(opts$outProj$short=="SIN") # maybe we should add other
        {
            opts$projPara <- "6371007.18 0.00 0.00 0.00 0.00 0.00 0.00 0.00 86400.00 0.00 0.00 0.00 0.00 0.00 0.00"
        } else 
        {
            cat("No output projection parameters specified. Reprojecting with no Parameters!\n")
            opts$projPara <- "0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0"
        }
    } else 
    {
        cat("Output projection parameters specified!\nUsing:\n ",opts$projPara,"\n")
    }

    for (z in 1:length(opts$product$PRODUCT))
    {
            
        if (opts$product$TYPE[z]=="CMG") 
        {
            tileID="GLOBAL"
            ntiles=1 
        } else 
        {
            opts$extent <- getTile(extent=opts$extent,tileH=opts$tileH,tileV=opts$tileV,buffer=opts$buffer)
            ntiles    <- length(opts$extent$tile)
        }
    
        todo <- paste(opts$product$PRODUCT[z],".",opts$product$CCC[[opts$product$PRODUCT[z]]],sep="")    
    
        for(u in 1:length(todo))
        {
            if (is.null(opts$job))
            {
                opts$job <- paste(todo[u],"_",format(Sys.time(), "%Y%m%d%H%M%S"),sep="")    
                cat("No 'job' name specified, generated (date/time based)):",opts$job,"\n")
            }
            outDir <- file.path(opts$outDirPath,opts$job,fsep="/")
            dir.create(outDir)

            ######################## along platform (TerraAqua)
            ftpdirs <- list()    ##  FRS: Fix provided by Ahmadou Dicko
            ftpdirs[[1]] <- as.Date(getStruc(product=strsplit(todo[u],"\\.")[[1]][1],collection=strsplit(todo[u],"\\.")[[1]][2],begin=tLimits$begin,end=tLimits$end,server=opts$MODISserverOrder[1])$dates)
            
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

            ######################### along begin -> end date
                for (l in 1:length(avDates))
                { 
                    files <- unlist(getHdf(product=opts$product$PRODUCT[z],collection=strsplit(todo[u],"\\.")[[1]][2],begin=avDates[l],end=avDates[l],tileH=opts$extent$tileH,tileV=opts$extent$tileV,stubbornness=opts$stubbornness))
                    
                    if (length(files)!=0)
                    {
                        mos <- opts$mosaic
        
                        if (mos)
                        {
                            # if not all files available switch "off" mosaicking and process single files. Problematic in areas with tiles outside land!
                            if (sum(file.exists(files)) < length(opts$extent$tile))
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
                            if (is.null(opts$SDSstring))
                            {
                                opts$SDSstring <- rep(1,length(getSds(HdfName=files[q],method="mrt")$SDSnames))
                            }    
                
                            SDSstringIntern <- getSds(HdfName=files[q],SDSstring=opts$SDSstring,method="mrt")
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
                                filename = file(paraname, open="wt")
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
                                Sys.sleep(0.5) # without wait the skript can break here. "wait" is a try but it seams to work!!!
                            }
            
                            basenam <- strsplit(files[q],"/")[[1]]
                            basenam <- basenam[length(basenam)]
        
                            if (mos)
                            {
                                basenam <- paste(strsplit(basenam,"\\.")[[1]][c(1,2,4)],collapse=".")
                            } else {
                                basenam <- paste(strsplit(basenam,"\\.")[[1]][c(1,2,3,4)],collapse=".")    
                            }
        
                            if (!opts$anonym)
                            {   
                                basenam <- paste(basenam,opts$job,sep=".")
                            }
    
                            #### Write prm File
                            paraname <- paste(outDir,"/MRTgResample.prm",sep="")
                            filename = file(paraname, open="wt")

                            if (mos)
                            {
                                write(paste('INPUT_FILENAME = "',outDir,"/",TmpMosNam,'"',sep=''), filename)
                            } else 
                            {
                                write(paste('SPECTRAL_SUBSET = ( ',SDSstringIntern$SDSstring,' )',sep=''), filename)
                                write(paste('INPUT_FILENAME = "',files[q],'"',sep=''), filename)
                            }
    
                            write('SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG',filename)
    
                            if (!is.null(opts$extent$extent))
                            {
                                write(paste('SPATIAL_SUBSET_UL_CORNER = (',opts$extent$extent@ymax,' ',opts$extent$extent@xmin,')',sep=''),filename)
                                write(paste('SPATIAL_SUBSET_LR_CORNER = (',opts$extent$extent@ymin,' ',opts$extent$extent@xmax,')',sep=''),filename)
                            }
                            if (opts$pixelSize!="asIn")
                            {
                                write(paste('OUTPUT_PIXEL_SIZE = ',opts$pixelSize,sep=''),filename) 
                            }    
                            write(paste('OUTPUT_FILENAME = ',outDir,"/",basenam,ext,sep=''),filename) 
                            write(paste('RESAMPLING_TYPE = ',opts$resamplingType,sep=''),filename)
                            
                            write(paste('OUTPUT_PROJECTION_TYPE = ',opts$outProj$short,sep=''),filename)
                            
                            if (opts$outProj$short=="UTM" && !is.null(opts$zone))
                            {
                                write(paste('UTM_ZONE = ',opts$zone,sep=''),filename)
                            }
        
                            if (!is.null(opts$projPara))
                            {
                                write(paste('OUTPUT_PROJECTION_PARAMETERS = ( ',opts$projPara,' )',sep=''),filename)
                            }
                            if (!is.null(opts$datum))
                            {
                                write(paste('DATUM =', opts$datum,sep=''),filename)
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
                    } else {
                        cat("Missing files on",avDates[l],"jumping to the next date",sep="\n")
                    }
                } # l, avDates
            } else {
                cat("No files found for",todo[u],"within the date range\n")
            }
        } # u   
    }
}

