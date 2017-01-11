#' List SDS Layers in an .HDF File
#' 
#' @description 
#' This function lists the names of all scientific datasets (SDS) contained in a 
#' specified MODIS grid HDF file.
#' 
#' @param HdfName \code{character}. (Absolute) filename from which to extract 
#' SDS names.
#' @param SDSstring \code{character}, see Value.
#' @param method \code{character}, defaults to \code{"gdal"}. Caution: on 
#' Windows, the default 'GDAL' installation doesn't support HDF4 files. Install 
#' 'FWTools' or use \code{method = "mrt"} instead.
#' 
#' @return 
#' A \code{list} or \code{character}. If \code{SDSstring} is provided, the 
#' function reports extracted SDS and a formatted SDSsting (e.g., "11101"). If 
#' not provided, the SDS names in \code{HdfName} are returned.  Consult the MRT 
#' manual for details.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' \dontrun{
#' getSds(HdfName="XXX.hdf")
#' getSds(HdfName="/path/XXX.hdf",method="gdal") # require GDAL (FWTools on Windows)
#' getSds(HdfName="/path/XXX.hdf",method="mrt") # require MRTool
#' }
#' 
#' @export getSds
#' @name getSds
getSds <- function(HdfName,SDSstring=NULL,method="gdal") 
{

    method <- toupper(method) 
    fsep <- .Platform$file.sep

    iw   <- getOption("warn") 
    options(warn=-1)
    on.exit(options(warn=iw))

    opts <- combineOptions()
    
    if (!file.exists(HdfName)) 
    {
        cat("Hm, I have to search for the file! Next time provide the full path and I'll be very fast!\n")
        HdfName <- list.files(path=opts$localArcPath,pattern=paste(HdfName,"$",sep=""),recursive=TRUE,full.names = TRUE)
    }
    
    HdfName <- HdfName[1]
    
    checkTool <- checkTools(tool=method,quiet=TRUE, opts = opts)[[method]][[method]]
    
    if (!checkTool)
    {
        stop("Method ",method, " does not work. Is ", method," installed properly on your system? Run: 'MODIS:::checkTools()' to check out which metods should work on your system!")
    }

    if (method=="GDAL")
    {
        if (.Platform$OS=="unix")
        {
            sdsRaw <- system(paste("gdalinfo ", HdfName,sep=""),intern=TRUE) 
        } else if (.Platform$OS=="windows")
        {
            usar <- gsub(utils::shortPathName(HdfName),pattern="\\\\",replacement="/")
            if (is.null(opts$gdalPath))
            {
                cmd <- paste('gdalinfo ', usar,sep="")
            } else
            {
                cmd <- shQuote(paste0(opts$gdalPath,'gdalinfo.exe ',usar),type="cmd")            
            }
             
            sdsRaw <- shell(cmd,intern=TRUE)
        }
        
        SDSnames <- grep(x=sdsRaw,pattern="SUBDATASET_[0-9]{1,2}_NAME",value=TRUE)
        SDSnames <- unlist(lapply(SDSnames,function(x) strsplit(x,"=")[[1]][2]))
        SDSnames <- unlist(lapply(SDSnames,function(x) gsub(x,pattern="\\\"",replacement="")))
        
        sds <- unlist(lapply(SDSnames,function(x) 
                {
                    x <- strsplit(x,":")[[1]]
                    x <- x[length(x)]                    
                }
            ))    
        
    } else if (method=="MRT")
    {
        if (.Platform$OS=="unix")
        {
            sdsRaw <- system(paste("sdslist",HdfName,sep=" "),intern=TRUE)
        
        }else if (.Platform$OS=="windows")
        {
            sdsRaw <- shell(gsub(fsep,"\\\\",paste('sdslist "',HdfName,'"',sep="")),intern=TRUE)
        }
        
        sds <- list()
        for (i in 1:length(sdsRaw))
        {
            sds[[i]] <- substr(sdsRaw[i],1,11) == "SDgetinfo: "
        }
        sds <- sdsRaw[unlist(sds)]
        sds <- unlist(lapply(sds,function(x){strsplit(x,", ")[[1]][2]}))

    }
    
    
    if (!is.null(SDSstring))
    {
        if (inherits(SDSstring,"list"))
        {
            SDSstring <- paste(SDSstring$SDSstring,collapse="")
        } else if (inherits(SDSstring,"numeric")) 
        {
            SDSstring <- paste(SDSstring,collapse="")
        }
    
        SDSstring <- gsub(pattern=" ",replacement="",x=SDSstring) # collapse the spaces

        if (nchar(SDSstring)!= length(sds)) 
        {
            warning("The file has ",length(sds)," layers (SDS), your SDSstring has length ",nchar(SDSstring),"!\nThe string is auto-completed!")
        }
                    
        msk <- rep(FALSE,length(sds))
        for (o in 1:length(sds))
        {
            msk[o] <- substr(SDSstring,o,o)==1
        }
    
        if (method=="GDAL") 
        {
            return(list(SDSnames = sds[msk],SDSstring = paste(as.numeric(msk),collapse=" "),SDS4gdal=SDSnames[msk]))
        } else 
        {
            return(list(SDSnames = sds[msk],SDSstring = paste(as.numeric(msk),collapse=" ")))
        }
    } else 
    {
    
        if (method=="GDAL") 
        {
            return(list(SDSnames = sds,SDS4gdal=SDSnames))
        } else 
        {
            return(list(SDSnames = sds))
        }
    }
}

