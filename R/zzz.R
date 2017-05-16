.onLoad <- function(lib, pkg) {
  
  ## leaflet.extras
  if (!"leaflet.extras" %in% utils::installed.packages() |
      utils::packageVersion("leaflet.extras") < '0.1.9009') {
    if ("leaflet.extras" %in% loadedNamespaces()) {
      detach("package:leaflet.extras", unload = TRUE)
    }
    
    devtools::install_github("bhaskarvk/leaflet.extras")
  }
  
  ## mapedit
  if (!"mapedit" %in% utils::installed.packages() |
      utils::packageVersion("mapedit") < '0.0.2') {
    if ("mapedit" %in% loadedNamespaces()) {
      detach("package:mapedit", unload = TRUE)
    }
    
    devtools::install_github("r-spatial/mapedit")
  }
}

.onAttach <- function(lib, pkg) {
  packageStartupMessage(MODISoptions(save=FALSE, checkTools=FALSE, quiet=TRUE))
}

