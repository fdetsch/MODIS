.onAttach <- function(lib, pkg) {
  packageStartupMessage(
    {
      jnk = suppressWarnings(
        suppressMessages(
          capture.output(
            MODISoptions(
              save = TRUE
              , checkTools = TRUE
              , quiet = TRUE
              , checkWriteDrivers = FALSE
              , ask = FALSE
              , check_earthdata_login = FALSE
            )
          )
        )
      )
      
      return(invisible())
    }
  )
}

