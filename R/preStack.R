# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2012
# Licence GPL v3

preStack <- function(pattern = "*", path = "./", files = NULL, timeInfo = NULL)
{
    if (is.null(files))
    {
        fnames <- list.files( path = path, pattern = pattern, full.names = TRUE)
    } else 
    {
        fnames <- files
    }
    
    if (length(fnames) == 0)
    {
        cat("No files found!\n") ; return(NULL)
    }
    
    if (!is.null(timeInfo))
    {
        avDates  <- extractDate( basename(fnames), pos1 = timeInfo$call$pos1, pos2 = timeInfo$call$pos2, format = timeInfo$call$format, asDate = TRUE)
        fnames   <- fnames[ order(avDates$inputLayerDates) ]
        avDates  <- sort(avDates$inputLayerDates)
        begin    <- min(timeInfo$inputLayerDates) 
        end      <- max(timeInfo$inputLayerDates)
        fnames   <- fnames[avDates >= begin & avDates <= end]        
    }

    cat("Found", length(fnames), "files!\n")
    if ( length(fnames) == 0 ) ( return(NULL) )
    
    return(fnames)
}





