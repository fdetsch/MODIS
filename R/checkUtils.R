checkEarthdataLogin = function(
  method = "auto"
  , path = "~/.netrc"
) {
  
  ## construct online file paths
  remote_urls = genString(
    "MCD64A1"
    , collection = "061"
    , date = "2019-12-01"
  )$remotePath
  
  ## cycle through download servers
  for (remote_url in remote_urls) {
    
    # try to download file from current server
    con = try(
      downloadFile(
        url = file.path(
          remote_url
          , "MCD64A1.A2019335.h32v11.061.2021309110404.hdf"
        )
        , destfile = tempfile(fileext = ".hdf")
        , method = method
        , path = path
        , quiet = TRUE
      )
      , silent = TRUE
    )
    
    # exit if download succeeded
    if (!inherits(con, "try-error")) {
      break
    }
  }
  
  ## return if download succeeded
  if (inherits(con, "try-error")) {
    msg = if (grepl("401", con)) {
      sprintf(
        paste(
          "Please check your Earthdata credentials in %s"
          , "or re-enter via `EarthdataLogin()`."
        )
        , path
      )
    }
    
    warning(
      gsub(
        "\n$"
        , ""
        , paste0(
          "Authentication failed with\n> "
          , tail(unlist(strsplit(as.character(con), split = "\\s+:\\s*")), 1L)
          , msg
        )
      )
      , call. = FALSE
    )
    
    FALSE
  } else {
    TRUE
  }
}
