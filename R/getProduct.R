#' Check and Create Product-Related Information
#' 
#' @description 
#' On user side, it is a funtion to find the desidered product. On package site, 
#' it generates central internal information to hande files.  
#' 
#' @param x \code{character}. MODIS filename, product name, regular expression 
#' passed to \code{pattern} in \code{\link{grep}}, or missing.
#' @param quiet \code{logical}, defaults to \code{FALSE}.
#' 
#' @return 
#' An invisible \code{list} with information usable by other functions or, if 
#' 'x' is missing, a \code{data.frame} with information about all products 
#' available.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' getProduct() # list available products
#' 
#' # or use regular expression style
#' getProduct("M.D11C3")
#' getProduct("M*D11C")
#' 
#' # or get information about specific product
#' internal_info <- getProduct("MOD11C3", quiet = TRUE) 
#' internal_info
#' 
#' @export getProduct
#' @name getProduct
getProduct <- function(x=NULL,quiet=FALSE) 
{    

#load(system.file("external", "MODIS_Products.RData", package="MODIS"))

    if (is.null(x)) { # if x isn't provided, return table of supported files.
      cls = c("SENSOR", "PRODUCT", "TOPIC", "PLATFORM","TYPE", "RES", "TEMP_RES")
      products = as.data.frame(MODIS_Products[cls])
      products = data.frame(products[order(products$PRODUCT), ]
                            , row.names = 1:nrow(products))
      
      return(products)
    }

    if (is.list(x) && names(x) %in% c("request", "PRODUCT", "TOPIC", "DATE", "TILE", "TILEV", "TILEH", "CCC", "PROCESSINGDATE", "FORMAT", "SENSOR", "PLATFORM", "PF1", "PF2", "PF3", "TOPIC", "TYPE", "RES", "TEMP_RES", "INTERNALSEPARATOR")) 
    {
        # if TRUE than it is a result from a getProduct() call. A good idea would be to have a CLASS for it!
        return(x)
    }
    
    ## moody but seams to work!!
    inbase  <- basename(x) # if x is a filename(+path) remove the path
    
    isProduct = any(sapply(inbase, function(i) grepl(i, getProduct()[, 2])))
    
    tmp = if (!isProduct) {
        isFile <- TRUE
        sapply(strsplit(inbase, "\\."), "[[", 1)
    } else {
        isFile <- FALSE
        inbase
    }

    product = sapply(tmp, function(i) skipDuplicateProducts(i, quiet = quiet))
    
    pattern <- sub(pattern="MXD", replacement="M.D", x=product, ignore.case=TRUE) # make a regEx out of "x"
    info <- listPather(MODIS_Products, 
                       grep(paste(pattern, collapse = "|")
                            , MODIS_Products$PRODUCT,ignore.case=TRUE))

    if (length(info$PRODUCT) == 0) {
      if (!quiet)
        cat("No product found with the name ", inbase
            , ". Try 'getProduct()' to list available products.\n", sep = "")
      
      return(NULL)
    }
    if (info$SENSOR[1]=="MODIS") 
    {
        info$PRODUCT <- toupper(info$PRODUCT)
    }
    
    if (isFile)
    { # in this case it must be a filename

      names(x) = "request"
      fname = getInfo(x, product = info$PRODUCT, type = info$TYPE)
      result <- c(x, fname, info)
      result <- result[!duplicated(names(result))]
      result <- as.list(sapply(result,function(x)as.character(x)))
      
      return(invisible(result))  
      
    } else  # if not a file
    {
        if (!quiet) 
        {
           for (i in seq_along(info$PRODUCT)) 
           {
               cat(paste(info$PRODUCT[i],'the',info$TEMP_RES[i],info$TYPE[i], info$TOPIC[i],'product from',info$SENSOR[i], info$PLATFORM[i],'with a ground resolution of', info$RES[i],'\n', sep = " "))
           }
        }

        if (info$SENSOR[1] == "MODIS") 
        {    
            PD <- substr(info$PRODUCT, 4, nchar(as.character(info$PRODUCT)))
            
            return(
                invisible(
                    list(request = inbase, PF1 = as.character(info$PF1),
                    PF2 = as.character(info$PF2), PF3 = as.character(info$PF3)
                    , PD = PD, PLATFORM = as.character(info$PLATFORM),
                    TYPE = as.character(info$TYPE), PRODUCT = as.character(info$PRODUCT),
                    SENSOR = as.character(info$SENSOR), SOURCE=info$SOURCE)
                )
            )
    
        } ## else if ... (add additional sensors)        
    }
}



