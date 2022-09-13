#' List SDS Layers in an HDF File
#' 
#' @description 
#' List the names of all scientific data sets (SDS) contained in a specified 
#' MODIS grid HDF file.
#' 
#' @param HdfName `character`. (Absolute) file name from which to extract SDS 
#'   names. Non-existing files are being looked up recursively in 
#'   `getOption("MODIS_localArcPath")`.
#' @param SDSstring An optional `character` string of 1s and 0s, see Value.
#' @param ... Currently not used.
#' 
#' @return 
#' A `list`. If 'SDSstring' is provided, the function reports matching SDS and a
#' formatted 'SDSstring' (e.g., "1 1 1 0 1"). If omitted, the names of all SDS 
#' in 'HdfName' are returned. 
#' 
#' @author 
#' Matteo Mattiuzzi, Florian Detsch
#' 
#' @examples
#' hdf = system.file(
#'   "external/MOD13A2.A2016145.h18v04.006.2016166145124.hdf"
#'   , package = "MODIS"
#' )
#' 
#' getSds(
#'   hdf
#' )
#' 
#' getSds(
#'   hdf
#'   , SDSstring = 1
#' )
#' 
#' @export getSds
#' @name getSds
getSds = function(
  HdfName
  , SDSstring = NULL
  , ...
) {
  
  if (!file.exists(HdfName)) {
      cat("Hm, I have to search for the file. Next time provide the full path and I'll be very fast!\n")
      HdfName = list.files(
          path = combineOptions()$localArcPath
          , pattern = paste0(HdfName, "$")
          , recursive = TRUE
          , full.names = TRUE
      )
  }
  
  SDSnames = unlist(sf::gdal_subdatasets(HdfName[1]))
  sds = gsub("\"", "", getSdsNames(SDSnames))
  
  if (!is.null(SDSstring))
  {
      if (inherits(SDSstring,"list"))
      {
          SDSstring <- paste(SDSstring$SDSstring,collapse="")
      } else if (inherits(SDSstring,"numeric")) 
      {
          SDSstring <- paste(SDSstring,collapse="")
      }
      
      SDSstring <- gsub(pattern=" ",replacement="",x=SDSstring) # collapse the spaces
      
      msk <- rep(FALSE,length(sds))
      for (o in 1:length(sds))
      {
          msk[o] <- substr(SDSstring,o,o)==1
      }
      
      return(list(SDSnames = sds[msk],SDSstring = paste(as.numeric(msk),collapse=" "),SDS4gdal=SDSnames[msk]))
  } else 
  {
      return(list(SDSnames = sds,SDS4gdal=SDSnames))
  }
}


getSdsNames = function(x) {
  x = strsplit(x, ":")
  mapply(`[`, x, lengths(x))
}
