#' List MODIS Quality Information
#' 
#' @description 
#' This function returns MODIS QA information for a specific product. It gets 
#' the information from an internal database and not all products are available.
#' 
#' @param product `character`, see [getProduct()].
#' @param what `character`. Parameter name, e.g. `"VI Quality"` for all MOD13
#'   products (see [MODIS Vegetation Index User's Guide](https://lpdaac.usgs.gov/documents/103/MOD13_User_Guide_V6.pdf), 
#'   Table 5, column "Parameter Name").
#' @param warn `logical`, whether or not to throw warning messages.
#' 
#' @return 
#' If `what = "all"` (default) a `data.frame`, else a `list`.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples
#' \dontrun{
#' detectBitInfo("MOD13Q1")
#' detectBitInfo("MOD13Q1", "VI usefulness")
#' 
#' detectBitInfo("MYD17A2")
#' }
#' 
#' @export detectBitInfo
#' @name detectBitInfo
detectBitInfo <- function(product, what='all',warn=TRUE)
{
  
  if(inherits(product,"Raster"))
  {
    product <- basename(names(product)[1])
  } else if(inherits(product,"character"))
  {
    product <- basename(product)
  } else
  {
    stop("Unknown input in detectBitInfo!")
  }
  
  product  <- strsplit(product,"\\.")[[1]][1]
  prodinfo <- getProduct(product,quiet=TRUE)@PRODUCT[1]
  if(is.null(prodinfo))
  {
    stop()
  } 
  
  try(info <- eval(parse(text=paste("",prodinfo,"_QC",sep=""))),silent=TRUE)
  
  if(exists("info"))
  {
    if(what!='all')
    {
      index <- grep(info$LongName,pattern=what, ignore.case = TRUE)
      res  <- list(bitShift=info[index,"bitShift"],bitMask=info[index,"bitMask"])
    } else 
    {
      res <- info
    }
  } else
  {
    if(warn)
    {
      warning("Could not detect 'bit' information, please provide me (matteo@mattiuzzi.com) the product name you have used so I can enable it, or add it manually see '?extractBits'!")
    }
    res <- NULL
  }
  return(res)    
}
