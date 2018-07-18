# .onLoad <- function(lib, pkg) {
#   
#   ## mapedit
#   avl <- length(find.package("mapedit", quiet = TRUE)) > 0
#   vld <- ifelse(avl, utils::packageVersion("mapedit") >= '0.0.2', FALSE)
#   
#   if (any(!avl, !vld)) {
#     if ("mapedit" %in% loadedNamespaces()) {
#       detach("package:mapedit", unload = TRUE)
#     }
#     
#     devtools::install_github("r-spatial/mapedit")
#   }
# }

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

