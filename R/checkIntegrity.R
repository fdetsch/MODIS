checkIntegrity <- function(x,...)
{
    opts <- combineOptions(...)

    iw   <- getOption("warn") 
    options(warn=-1)
    on.exit(options(warn=iw))

    if(!opts$gdalOk)
    {
        warning("Something wrong with your GDAL installation, see '?MODISoptions' for more details")
    } else
    {
        cmd <- paste0(opts$gdalPath,"gdalinfo ") 
        out <- rep(NA,length(x))

        for (i in seq_along(x))
        {
            if (basename(x[i])=="NA" | is.na(basename(x[i])))
            {
                out[i] <- NA
            } else
            {
                if (dirname(x[i])==".")
                {
                    x[i] <- paste0(genString(x=x[i],remote=FALSE,...)$localPath, basename(x[i]))        
                }
        
                if (!file.exists(x[i]))
                {
                    out[i] <- NA
                } else
                {
                    try(a <- system(paste0(cmd,correctPath(x[i],isFile=TRUE)), intern=TRUE), silent=TRUE)
                
                    if(length(grep(a,pattern="gdalinfo failed")==1) | length(a)==0)
                    {
                        out[i] <- FALSE
                    } else 
                    {
                        out[i] <- TRUE
                    }
                }
            }
        }
    return(out)
    }
}
       
    
