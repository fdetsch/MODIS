#' Create File with Earthdata Login Credentials
#' 
#' @description 
#' Create a hidden .netrc file with Earthdata login credentials in your home 
#' directory. If your priority server for MODIS file download is LPDAAC (see 
#' also \code{\link{MODISoptions}}), these are subsequently used to 
#' automatically login to \url{urs.earthdata.nasa.gov} and download required 
#' files.
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
#' \item{\url{http://docs.opendap.org/index.php/DAP_Clients_-_Authentication#LDAP} 
#' (section 2.2)}
#' \item{\url{https://github.com/MatMatt/MODIS/issues/10}}
#' }
#' 
#' @examples 
#' \dontrun{
#' lpdaacLogin()
#' }
#' 
#' @export lpdaacLogin
#' @name lpdaacLogin
lpdaacLogin <- function(server = "LPDAAC") {
  
  ## create file
  cat("Creating hidden file '~/.netrc' with login credentials...\n")
  netfile <- file("~/.netrc", open = "wt")
  
  ## server (in the long run, we should maybe create a look-up table for that)
  machine <- if (server == "LPDAAC") {
    "urs.earthdata.nasa.gov"
  } else {
    stop("Server '", server, "' currently not supported.\n")
  }
  
  write(paste("machine", machine), netfile)
  
  ## username
  x <- readline("Insert your Earthdata USERNAME: ")  
  write(paste("login", x), netfile)
  
  ## password
  x <- readline("Insert your Earthdata PASSWORD: ")  
  write(paste("password", x), netfile)
  
  ## change permissions
  close(netfile) 
  Sys.chmod('~/.netrc', mode = "600", use_umask = TRUE)
  
  return(invisible())
}