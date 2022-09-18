#' Add New Product to MODIS Collections
#' 
#' @description 
#' `addCollection()` is a non-exported helper function to add a new product 
#' column to the product collections managed by **MODIS** (see 
#' `MODIS:::collections`). Once added, the specified product will be tracked 
#' and, if required, kept up-to-date by [getCollection()].
#' 
#' @param product `character`. Name of the product that should be added to the 
#'   'collections' data set, see [getCollection()]. 
#' @param collection `numeric`. Optional information about available 
#'   collections. If not supplied, this defaults to `NA` and the user is 
#'   required to manually retrieve information about available collections via 
#'   `getCollection(..., forceCheck = TRUE)`. Note that the latter operation 
#'   requires the previous execution of `MODIS:::addProduct()` and 
#'   `MODIS:::addServer()` to make the added product available to 
#'   [getCollection()].
#' @param path_ext `character`. Path to folder containing file 
#'   'MODIS_Products.RData'. When working with RStudio projects (.Rproj), this 
#'   usually defaults to 'inst/external'.
#' @param overwrite `logical`. If `TRUE`, the initial '.RData' file located in 
#'   'path_ext' will be overwritten.
#' @param ... Currently not used.
#' 
#' @return 
#' A `data.frame` which, for each product featured by **MODIS**, holds 
#' information about available collections.
#' 
#' @seealso 
#' `MODIS:::collections`.
#' 
#' @author 
#' Florian Detsch
#' 
#' @examples 
#' \dontrun{
#' ## E.g., add collection of MODIS evapotranspiration product
#' MODIS:::addCollection(product = "MOD16A2", collection = 105)
#' }
#' 
# #' @export addCollection                     
#' @name addCollection
addCollection <- function(product, collection = NA, 
                          path_ext = "inst/external", overwrite = FALSE, ...) {
  
  ## load list of current products
  load(paste0(path_ext, "/collections.RData"))
  
  ## id of last and new entry
  int_id_last <- ncol(MODIScollection)
  int_id_new <- int_id_last + 1
  
  ## if missing, append NA entries to 'collection'
  int_len <- length(collection)
  if (int_len < nrow(MODIScollection))
    collection[(int_len+1):nrow(MODIScollection)] <- NA
  
  ## add new collection
  MODIScollection <- cbind(MODIScollection, collection)
  names(MODIScollection)[int_id_new] <- product

  ## output storage
  if (overwrite) {
    file_out <- paste0(path_ext, "/collections.RData")
    save(MODIScollection, file = file_out)
  }
  
  ## return updated collections dataset
  return(MODIScollection)
}
  
