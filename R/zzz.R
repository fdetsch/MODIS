.onLoad <- function(lib, pkg) {
  
  ## leaflet.extras
  avl <- length(find.package("leaflet.extras", quiet = TRUE)) > 0
  vld <- ifelse(avl, utils::packageVersion("leaflet.extras") >= '0.1.9009', FALSE)

  if (any(!avl, !vld)) {
    if ("leaflet.extras" %in% loadedNamespaces()) {
      detach("package:leaflet.extras", unload = TRUE)
    }
    
    devtools::install_github("bhaskarvk/leaflet.extras")
  }
  
  ## mapedit
  avl <- length(find.package("mapedit", quiet = TRUE)) > 0
  vld <- ifelse(avl, utils::packageVersion("mapedit") >= '0.0.2', FALSE)
  
  if (any(!avl, !vld)) {
    if ("mapedit" %in% loadedNamespaces()) {
      detach("package:mapedit", unload = TRUE)
    }
    
    devtools::install_github("r-spatial/mapedit")
  }
}

.onAttach <- function(lib, pkg) {
  packageStartupMessage(MODISoptions(save=FALSE, checkTools=FALSE, quiet=TRUE))
}

