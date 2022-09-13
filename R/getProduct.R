#' Check and Create Product-Related Information
#' 
#' @description 
#' On user side, it is a function to find the desired product. On package site,
#' it generates central internal information to handle files.
#' 
#' @param x `character`. MODIS file name, product name, regular expression 
#'   passed as pattern to [grep()], or missing. Use dot notation to address 
#'   Terra and Aqua products at the same time, e.g. `"M.D13Q1"`. 
#' @param quiet `logical`, defaults to `FALSE`.
#' @param ... Additional arguments passed to [getCollection()].
#' 
#' @return 
#' If 'x' is missing, a `data.frame` with information about all MODIS products 
#' available. In case of `character` input, an invisible [MODISproduct-class] or
#' [MODISfile-class] object depending on the type of input (product, regular 
#' expression or file name); the object holds information usable by other 
#' functions. 
#' 
#' @author 
#' Matteo Mattiuzzi and Florian Detsch
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
#' # or use a valid filename
#' fileinfo <- getProduct("MYD11A1.A2009001.h18v04.006.2015363221538.hdf")
#' fileinfo
#' 
#' @export getProduct
#' @name getProduct
getProduct <- function(x = NULL, quiet = FALSE, ...) 
{    

#load(system.file("external", "MODIS_Products.RData", package="MODIS"))

    if (is.null(x)) { # if x isn't provided, return table of supported files.
      cls = c("PRODUCT", "TOPIC", "PLATFORM","TYPE", "RES", "TEMP_RES")
      products = as.data.frame(MODIS_Products[cls])
      products = data.frame(products[order(products$PRODUCT), ]
                            , row.names = 1:nrow(products))
      
      return(products)
    }

    if (inherits(x, "MODISproduct")) 
    {
        # if TRUE then it is a result from a getProduct() call. 
        return(x)
    }
    
    ## moody but seams to work!!
    inbase  <- basename(x) # if x is a filename(+path) remove the path
    inbase = gsub("\\.[[:digit:]]{3}", "", inbase)
    
    isProduct = any(sapply(inbase, function(i) {
      grepl(gsub(" ", "", i), getProduct()[, 1])
    }))
    
    tmp = if (!isProduct) {
        isFile <- TRUE
        sapply(strsplit(inbase, "\\."), "[[", 1)
    } else {
        isFile <- FALSE
        gsub(" ", "", inbase) ## if 'isProduct', remove whitespaces
    }

    product = sapply(tmp, function(i) skipDuplicateProducts(i, quiet = quiet))
    
    pattern <- sub(pattern="MXD", replacement="M.D", x=product, ignore.case=TRUE) # make a regEx out of "x"
    
    ids = do.call(c, lapply(pattern, function(i) {
      grep(i, MODIS_Products$PRODUCT, ignore.case = TRUE)
    }))
    
    if (length(ids) == 0) {
      if (!quiet)
        cat("No product found with the name ", inbase
            , ". Try 'getProduct()' to list available products.\n", sep = "")
      
      return(invisible(NULL))
    } else {
      info <- listPather(MODIS_Products, ids)
    }

    info$PRODUCT <- toupper(info$PRODUCT)

    if (isFile)
    { # in this case it must be a filename

      fname = getInfo(x, product = info$PRODUCT, type = info$TYPE)
      result <- c(x, fname, info)
      result <- result[!duplicated(names(result))]
      
      out = methods::new("MODISfile"
                         , request = x
                         , PRODUCT = fname$PRODUCT
                         , DATE = fname$DATE
                         , TILE = fname$TILE
                         , CCC = fname$CCC
                         , PROCESSINGDATE = fname$PROCESSINGDATE
                         , FORMAT = fname$FORMAT
                         , SENSOR = info$SENSOR
                         , PLATFORM = info$PLATFORM
                         , PF1 = info$PF1
                         , PF2 = info$PF2
                         , PF3 = info$PF3
                         , PF4 = info$PF4
                         , TYPE = result$TYPE
                         , SOURCE = result$SOURCE
                         , POS1 = as.integer(result$POS1)
                         , POS2 = as.integer(result$POS2))

    } else  # if not a file
    {
        if (!quiet) 
        {
           for (i in seq_along(info$PRODUCT)) 
           {
               cat(paste(info$PRODUCT[i],'the',info$TEMP_RES[i],info$TYPE[i], info$TOPIC[i],'product from MODIS', info$PLATFORM[i],'with a ground resolution of', info$RES[i],'\n', sep = " "))
           }
        }

      PD <- substr(info$PRODUCT, 4, nchar(as.character(info$PRODUCT)))
      
      out = methods::new("MODISproduct"
                         , request = x
                         , PF1 = as.character(info$PF1)
                         , PF2 = as.character(info$PF2)
                         , PF3 = as.character(info$PF3)
                         , PF4 = as.character(info$PF4)
                         , PD = PD
                         , PLATFORM = as.character(info$PLATFORM)
                         , TYPE = as.character(info$TYPE)
                         , PRODUCT = as.character(info$PRODUCT)
                         , SENSOR = as.character(info$SENSOR)
                         , SOURCE = info$SOURCE
      )
      
      out@CCC = getCollection(out, quiet = TRUE, ...)
    }
    
    names(out@SOURCE) = out@PRODUCT
    return(invisible(out))
}
