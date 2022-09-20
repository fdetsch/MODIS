#' Create File with Earthdata Login Credentials
#' 
#' @description 
#' Create a hidden .netrc file with Earthdata Login credentials in your home 
#' directory. If your priority server for MODIS file download is LP DAAC (see 
#' also [MODISoptions()]), these are subsequently used to automatically login to
#' <https://urs.earthdata.nasa.gov/> and download required files.
#' 
#' @param server `character`. MODIS file server, defaults to `"LPDAAC"` which is
#'   currently the only option available.
#' 
#' @return 
#' Invisible.
#' 
#' @author 
#' Matteo Mattiuzzi and Florian Detsch
#' 
#' @seealso
#' * <https://docs.opendap.org/index.php/DAP_Clients_-_Authentication#LDAP>
#'   (section 2.2)
#' * <https://github.com/fdetsch/MODIS/issues/10>
#' 
#' @examples 
#' \dontrun{
#' lpdaacLogin()
#' }
#' 
#' @name lpdaacLogin-deprecated
#' @usage lpdaacLogin(server = "LPDAAC")     
#' @seealso [MODIS-deprecated]
#' @keywords internal
NULL

#' @rdname MODIS-deprecated
#' @section `lpdaacLogin`:
#' For [lpdaacLogin()], use [EarthdataLogin()] instead.
#' 
#' @export 
lpdaacLogin <- function(server = "LPDAAC") {
  
  .Deprecated("EarthdataLogin")
  
  #server = ifelse(server %in% c("LPDAAC", "LAADS"), "Earthdata", server)
  #return(EarthdataLogin(service = server))
  return(EarthdataLogin())
}
