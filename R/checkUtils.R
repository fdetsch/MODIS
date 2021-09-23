checkEarthdataLogin = function(
  method = NULL
) {
  
  ## credentials
  crd = credentials()
  usr = crd$login; pwd = crd$password
  
  ## if not available, add entry to ~/.netrc
  if (
    is.null(usr) || usr == "" ||
    is.null(pwd) || pwd == ""
  ) {
    crd = EarthdataLogin()
    usr = crd$login; pwd = crd$password
  }
  
  
  ### `download.file()` ----
  
  if (is.null(method)) {
    method = "auto"
  }
  
  if (!method %in% c("wget", "curl")) {
    
    cmd = try(system("wget -h", intern = TRUE), silent = TRUE)
    method = "wget"
    
    if (inherits(cmd, "try-error")) {
      cmd = try(system("curl -h", intern = TRUE), silent = TRUE)
      method = "curl"
    }
    
    if (inherits(cmd, "try-error")) {
      stop("Make sure either 'wget' or 'curl' is available in "
           , "order to download data from LP DAAC.")
    }
  }
  
  ## cookies
  ofl = file.path(tempdir(), ".cookies.txt")
  if (!file.exists(ofl))
    jnk = file.create(ofl)
  on.exit(file.remove(ofl))
  
  ## set download extras for wget ..
  extra = if (method == "wget") {
    paste("--user", usr, "--password", pwd
          , "--load-cookies", ofl
          , "--save-cookies", ofl
          , "--keep-session-cookie --no-check-certificate")
    
  ## .. or curl
  } else {
    paste('--user', paste(usr, pwd, sep = ":")
          , '-k -L -c', ofl, '-b', ofl)
  }
  
  ## try to download lpdaac test file
  wrn = getOption("warn")
  options(warn = 1)
  
  txt = utils::capture.output(
    con <- try(
      download.file(
        url = "https://e4ftl01.cr.usgs.gov/MOTA/MCD64A1.006/2019.12.01/MCD64A1.A2019335.h32v11.006.2020036042913.hdf"
        , destfile = tempfile(fileext = ".hdf")
        , mode = 'wb'
        , method = method
        , quiet = TRUE
        , extra = extra)
      , silent = TRUE
    )
    , type = "message"
  )
  
  options(warn = wrn)  
  
  ## return if download succeeded
  if (inherits(con, "try-error")) {
    warning(
      "Authentication failed, please check your Earthdata credentials in "
      , "~/.netrc or re-enter via `EarthdataLogin()`."
    )
    FALSE
  } else {
    TRUE
  }
}
