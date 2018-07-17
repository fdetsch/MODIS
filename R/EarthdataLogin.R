#' Create File with Earthdata Login Credentials
#' 
#' @description 
#' Create a hidden \code{.netrc} file with Earthdata login credentials in your 
#' home directory. The information included therein is used to login to 
#' \url{urs.earthdata.nasa.gov} which is a mandatory requirement in order to 
#' download MODIS data from LPDAAC, LAADS and NSIDC (see also 
#' \code{\link{MODISoptions}}).
#' 
#' @param service \code{character}. Search service for MODIS data, defaults to 
#' \code{"Earthdata"} which is currently the only option available.
#' @param usr,pwd Login credentials as \code{character}. If \code{NULL}, 
#' username and password are read from the terminal.
#' 
#' @return 
#' An \code{invisible()} absolute file path of the created \code{.netrc} file as 
#' \code{character}.
#' 
#' @author 
#' Matteo Mattiuzzi and Florian Detsch
#' 
#' @seealso 
#' \itemize{
#' \item{\url{http://docs.opendap.org/index.php/DAP_Clients_-_Authentication#LDAP} 
#' (section 2.2)}
#' \item{\url{https://github.com/MatMatt/MODIS/issues/10}}
#' \item{\url{https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget}}
#' }
#' 
#' @examples 
#' \dontrun{
#' EarthdataLogin()
#' }
#' 
#' @export EarthdataLogin
#' @name EarthdataLogin
EarthdataLogin <- function(service = "Earthdata", usr = NULL, pwd = NULL) {
  
  ## create file
  cat("Creating hidden file '~/.netrc' with login credentials...\n")
  netfile <- file("~/.netrc", open = "wt")
  
  ## service (in the long run, if services other than Earthdata are affected, we 
  ## should probably create a look-up table for that)
  machine <- if (service == "Earthdata") {
    "urs.earthdata.nasa.gov"
  } else {
    stop("Server '", service, "' currently not supported.\n")
  }
  
  write(paste("machine", machine), netfile)
  
  ## username
  if (is.null(usr)) {
    usr <- readline("Insert your Earthdata USERNAME: ")  
  }
  write(paste("login", usr), netfile)
  
  ## password
  if (is.null(pwd)) {
    pwd <- readline("Insert your Earthdata PASSWORD: ")  
  }
  write(paste("password", pwd), netfile)
  
  ## change permissions
  close(netfile) 
  Sys.chmod('~/.netrc', mode = "600", use_umask = TRUE)
  
  return(invisible(normalizePath('~/.netrc')))
}