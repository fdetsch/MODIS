#' Create File with Earthdata Login Credentials
#' 
#' @description 
#' Create a hidden .netrc file with Earthdata login credentials in your home 
#' directory. If your priority server for MODIS file download is LP DAAC (see 
#' also \code{\link{MODISoptions}}), these are subsequently used to 
#' automatically login to \url{https://urs.earthdata.nasa.gov/} and download 
#' required files.
#' 
#' @param server \code{character}. MODIS file server, defaults to 
#' \code{"LPDAAC"} which is currently the only option available.
#' 
#' @return 
#' \code{invisible()}.
#' 
#' @author 
#' Matteo Mattiuzzi and Florian Detsch
#' 
#' @seealso 
#' \itemize{
#' \item{\url{https://docs.opendap.org/index.php/DAP_Clients_-_Authentication#LDAP} 
#' (section 2.2)}
#' \item{\url{https://github.com/MatMatt/MODIS/issues/10}}
#' }
#' 
#' @examples 
#' \dontrun{
#' lpdaacLogin()
#' }
#' 
#' @name lpdaacLogin-deprecated
#' @usage lpdaacLogin(server = "LPDAAC")     
#' @seealso \code{\link{MODIS-deprecated}}  
#' @keywords internal
NULL

#' @rdname MODIS-deprecated
#' @section \code{lpdaacLogin}:
#' For \code{lpdaacLogin}, use \code{\link{EarthdataLogin}} instead.
#' 
#' @export 
lpdaacLogin <- function(server = "LPDAAC") {
  
  .Deprecated("EarthdataLogin")
  
  #server = ifelse(server %in% c("LPDAAC", "LAADS"), "Earthdata", server)
  #return(EarthdataLogin(service = server))
  return(EarthdataLogin())
}