#' Organize (MODIS) Files in Preparation for Stacking
#' 
#' @description 
#' This function lets you sort a vector of file names according to date. It is 
#' thought to be used on results from [runGdal()] or [runMrt()].
#' 
#' @param pattern Regular expression passed to [list.files()]
#' @param path `character`. Location of MODIS files to stack.
#' @param files `character` vector of file names. If provided, arguments 
#'   'pattern' and 'path' are ignored.
#' @param timeInfo Output from [orgTime()].
#' 
#' @return 
#' A `character` vector of file names within the query. If 'timeInfo' is 
#'   provided, file names are sorted and subsetted by date.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' # see Examples in ?smooth.spline.raster
#' 
#' @export preStack
#' @name preStack
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
