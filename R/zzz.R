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
    x <- capture.output(MODISoptions(save=FALSE, checkTools=FALSE))
    rm(x)
  })
}

