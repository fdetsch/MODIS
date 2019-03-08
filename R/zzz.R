.onAttach <- function(lib, pkg) {
  packageStartupMessage({
    suppressWarnings(
      suppressMessages(
        jnk <- capture.output(
          MODISoptions(save = TRUE, checkTools = TRUE, quiet = TRUE
                       , checkWriteDrivers = FALSE, ask = FALSE)
        )
      )
    )
    
    return(invisible())
  })
}

