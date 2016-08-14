#.onLoad <- function(lib, pkg)
#{
#    win <- options("warn")
#    options(warn=-1)
#    MODISoptions(save=FALSE, checkPackages=FALSE, quiet=TRUE)
#    options(warn=win$warn)
#}
.onAttach <- function(lib, pkg)
{
    #win <- options("warn")
    #options(warn=-1)
    packageStartupMessage(MODISoptions(save=FALSE, checkPackages=FALSE, quiet=TRUE))
    #packageStartupMessage(c("MODIS_manual: https://ivfl-rio.boku.ac.at/owncloud/public.php?service=files&t=660dc830afb091237cc40b3dea2fdf6b\n",MODISoptions(save=FALSE, checkPackages=FALSE, quiet=TRUE)))
}

