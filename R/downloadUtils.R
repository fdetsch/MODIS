downloadFile = function(
  url
  , destfile
  , method
  , path = "~/.netrc"
  , quiet = FALSE
) {
  
  ## get or set credentials
  crd = credentials(
    path = path
  )
  
  usr = crd$login
  pwd = crd$password
  
  if (
    is.null(usr) || usr == "" ||
    is.null(pwd) || pwd == ""
  ) {
    
    crd = EarthdataLogin(
      path = path
    )
    
    usr = crd$login
    pwd = crd$password
  }
  
  ## cookies file
  cks = file.path(
    tempdir()
    , ".cookies.txt"
  )
  
  if (!file.exists(cks)) {
    jnk = file.create(cks)
  }
  
  ## if `dlmethod = "auto"`, attempt to find curl or wget
  if (method == "auto") {
    
    cmd = try(system("curl -h", intern = TRUE), silent = TRUE)
    method = "curl"
    
    if (inherits(cmd, "try-error")) {
      cmd = try(system("wget -h", intern = TRUE), silent = TRUE)
      method = "wget"
    }
    
    if (inherits(cmd, "try-error")) {
      stop(
        "Make sure either curl or wget is available in order to download data."
        , call. = FALSE
      )
    }
  }
  
  
  ### curl ----
  
  if (method == "curl") {
    return(
      downloadFileCurl(
        url
        , destfile
        , usr
        , pwd
        , cookies = cks
        , quiet = quiet
      )
    )
  }
  
  
  ### other ----
  
  # TODO: aria2 integration
  
  ## early exit: single quote in password
  if (grepl("'", pwd)) {
    stop(
      "Earthdata passwords used with this package must not contain single "
      , "quotes when download method is other than 'curl'."
      # , call. = FALSE
    )
  }
  
  ## if applicable, set wget extras
  extra = if (method == "wget") {
    sprintf(
      paste(
        "--user '%s'"
        , "--password '%s'"
        , "--load-cookies %s"
        , "--save-cookies %s"
        , "--keep-session-cookie"
        , "--no-check-certificate"
      )
      , usr
      , pwd
      , cks
      , cks
    )
  }
  
  ## download
  utils::download.file(
    url = url
    , destfile = destfile
    , mode = 'wb'
    , method = method
    , quiet = quiet
    , cacheOK = TRUE
    , extra = extra # `NULL` if not wget
  )
}


downloadFileCurl = function(
  url
  , destfile
  , usr
  , pwd
  , cookies
  , quiet = FALSE
) {
  
  ## determine download server from url
  srv = if (grepl("^https://e4ftl01.cr.usgs.gov", url)) {
    "LPDAAC"
  } else if (grepl("^https://ladsweb.modaps.eosdis.nasa.gov", url)) {
    "LAADS"
  } else if (grepl("^https://n5eil01u.ecs.nsidc.org", url)) {
    "NSIDC"
  }
  
  ## set up curl handle
  h = curl::new_handle()
  
  args = list(
    handle = h
    , userpwd = paste0(usr, ":", pwd)
    , httpauth = 1L
    , cookiefile = cookies # read
    , cookiejar = cookies # write
    , connecttimeout = 60L
  )
  
  if (srv != "LAADS") {
    args$httpauth = NULL
  }
  
  do.call(
    curl::handle_setopt
    , args = args
  )
  
  ## download
  jnk = curl::curl_download(
    url
    , destfile
    , quiet = quiet
    , handle = h
  )
  
  ## imitate download.file() return value (i.e. 0 = success, non-zero = failure)
  as.integer(
    !file.exists(
      jnk
    )
  )
}
