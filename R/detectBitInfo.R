#' List MODIS Quality Information
#' 
#' @description 
#' This function returns MODIS QA information for a specific product. It gets 
#' the information from an internal database and not all products are available.   
#' 
#' @param product \code{character}, see \code{\link{getProduct}}.
#' @param what \code{character}. Parameter name, i.e. 
#' \url{https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1}, 
#' (TABLE 2: MOD13Q1 VI Quality; Long Name).
#' @param warn \code{logical}, whether or not to throw warning messages.
#' 
#' @return 
#' If \code{what = "all"} a \code{data.frame}, else a \code{list}.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' detectBitInfo("MOD13Q1")
#' detectBitInfo("MOD13Q1", "VI usefulness")
#' 
#' detectBitInfo("MYD17A2")
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
  prodinfo <- getProduct(product,quiet=TRUE)$PRODUCT[1]
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
