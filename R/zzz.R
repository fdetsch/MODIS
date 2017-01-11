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
    packageStartupMessage(MODISoptions(save=FALSE, checkTools=FALSE, quiet=TRUE))
}

