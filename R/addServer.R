#' Add New Remote Server to MODIS Inventory
#' 
#' @description 
#' \code{addServer} is a non-exported helper function to add a new entry to the 
#' list of online (FTP) servers featured by \strong{MODIS} (see 
#' \code{MODIS:::MODIS_FTPinfo}).
#' 
#' @param name Character. Name of the remote server that should be added to the 
#' inventory. 
#' @param sensor Character. Sensor type, defaults to 'MODIS'.
#' @param basepath Character. Absolute online server path.
#' @param varpath Character. Pattern of organizational structure on server.
#' @param content Character. Content type, defaults to "images".
#' @param path_ext Character. Path to folder containing file 
#' 'MODIS_FTPinfo.RData'. When working with RStudio projects (.Rproj), this 
#' usually defaults to 'inst/external'.
#' @param overwrite Logical. If \code{TRUE}, the initial '.RData' file located 
#' in 'path_ext' will be overwritten.
#' @param ... Currently not used.
#' 
#' @return 
#' A 'list' holding the updated contents of 'MODIS_FTPinfo.RData'.
#' 
#' @seealso 
#' \code{MODIS:::MODIS_FTPinfo}.
#' 
#' @author 
#' Florian Detsch
#' 
#' @examples 
#' \dontrun{
#' ## E.g., add server of MODIS evapotranspiration product
#' MODIS:::addServer(name = "NTSG", sensor = "MODIS",
#'                   basepath = "ftp://ftp.ntsg.umt.edu/pub/MODIS/NTSG_Products/MOD16/",
#'                   varpath = "PRODUCT.CCC/YYYY/DDD/")
#' }                   
#'     
# #' @export addServer                 
#' @name addServer
addServer <- function(name, sensor = "MODIS", basepath, varpath, 
                      content = "images", path_ext = "inst/external", 
                      overwrite = FALSE, ...) {
  
  ## load list of current products
  load(paste0(path_ext, "/MODIS_FTPinfo.RData"))
  
  ## id of last and new entry
  int_id_last <- length(MODIS_FTPinfo)
  int_id_new <- int_id_last + 1
  
  ## add new collection
  ls_new <- list(list(
    name = name,
    SENSOR = sensor, 
    basepath = basepath, 
    variablepath = varpath, 
    content = content
  ))
  names(ls_new) <- paste0("ftpstring", int_id_new)

  MODIS_FTPinfo <- append(MODIS_FTPinfo, ls_new)
    
  ## output storage
  if (overwrite) {
    file_out <- paste0(path_ext, "/MODIS_FTPinfo.RData")
    save(MODIS_FTPinfo, file = file_out)
  }
  
  ## return updated collections dataset
  return(MODIS_FTPinfo)
}