#' Add New Product to MODIS Inventory
#' 
#' @description 
#' \code{addProduct} is a non-exported helper function to add a new entry to the 
#' list of satellite products featured by \strong{MODIS} (see 
#' \code{MODIS:::MODIS_Products}).
#' 
#' @param product Character. Name of the product that should be added to the 
#' inventory, see \code{\link{getProduct}}.
#' @param sensor Character. Sensor type, defaults to 'MODIS'.
#' @param platform Character. Satellite platform on which the specified 'sensor' 
#' is mounted, defaults to "Terra".
#' @param pf1,pf2 Character. Online server paths.
#' @param topic Character. The official name of 'product'.
#' @param type Character. Product type, defaults to 'Tile'.
#' @param res Character. Spatial resolution of 'product', e.g. "1000m".
#' @param temp_res Character. Temporal resolution of 'product', e.g. "8 Day".
#' @param internalseparator Character. Separator string matching the product's 
#' naming convention, defaults to '\\.' for MODIS products.
#' @param server Character. Server to download the data from (more than one 
#' entry is possible).
#' @param path_ext Character. Path to folder containing file 
#' 'MODIS_Products.RData'. When working with RStudio projects (.Rproj), this 
#' usually defaults to 'inst/external'.
#' @param overwrite Logical. If \code{TRUE}, the initial '.RData' file located 
#' in 'path_ext' will be overwritten.
#' @param ... Currently not used.
#' 
#' @return 
#' A 'list' holding the updated contents of file 'MODIS_Products.RData'.
#' 
#' @author 
#' Florian Detsch
#' 
#' @seealso 
#' \code{MODIS:::MODIS_Products}, \code{\link{getProduct}}.
#' 
#' @examples 
#' \dontrun{
#' ## E.g., add MODIS evapotranspiration product
#' MODIS:::addProduct(product = "MOD16A2", sensor = "MODIS", platform = "Combined", 
#'                    pf1 = "MOLT", pf2 = "MOD", res = "1000m", temp_res = "8 Day", 
#'                    topic = "Global Terrestrial Evapotranspiration", server = "NTSG")
#' }
#'      
# #' @export addProduct                   
#' @name addProduct
addProduct <- function(product, sensor = "MODIS", platform = c("Terra", "Aqua"), 
                       pf1, pf2, topic, type = c("Tile", "Swath", "CMG"), res, 
                       temp_res, internalseparator = "\\.", 
                       server = c("LPDAAC", "LAADS"), 
                       path_ext = "inst/external", overwrite = FALSE, ...) {
  
  ## load list of current products
  load(paste0(path_ext, "/MODIS_Products.RData"))
  
  ## factor to character conversion
  log_was_factor <- logical(length = length(MODIS_Products))
  for (i in seq(MODIS_Products)) {
    if (is.factor(MODIS_Products[[i]])) {
      log_was_factor[i] <- TRUE
      MODIS_Products[[i]] <- as.character(MODIS_Products[[i]])
    }
  }
  
  ## id of last and new entry
  int_len_all <- sapply(MODIS_Products, length)
  int_id_last <- unique(int_len_all)
  int_id_new <- int_id_last + 1
  
  ## add new product
  MODIS_Products$SENSOR[int_id_new] <- sensor
  MODIS_Products$PRODUCT[int_id_new] <- product
  MODIS_Products$PLATFORM[int_id_new] <- platform[1]
  MODIS_Products$PF1[int_id_new] <- pf1
  MODIS_Products$PF2[int_id_new] <- pf2
  MODIS_Products$TOPIC[int_id_new] <- topic
  MODIS_Products$TYPE[int_id_new] <- type[1]
  MODIS_Products$RES[int_id_new] <- res
  MODIS_Products$TEMP_RES[int_id_new] <- temp_res
  MODIS_Products$INTERNALSEPARATOR[int_id_new] <- internalseparator
  MODIS_Products$SOURCE <- append(MODIS_Products$SOURCE, list(server))
  
  ## character to factor conversion
  for (i in seq(MODIS_Products)) {
    if (log_was_factor[i]) {
      MODIS_Products[[i]] <- as.factor(MODIS_Products[[i]])
    }
  }
  
  ## output storage (optional)
  if (overwrite) {
    file_out <- paste0(path_ext, "/MODIS_Products.RData")
    save(MODIS_Products, file = file_out)
  }
  
  ## return updated list of products
  return(MODIS_Products)
}
