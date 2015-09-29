# lazy loading
##########################################

tiletable <- read.table(system.file("external", "tiletable.txt", package="MODIS"), header=TRUE)

# save(tileNames,file="~/tileNames.RData") # in chase of changes
load(system.file("external", "tileNames.RData", package="MODIS"))

# load MODIS Tiles (sr)
load(system.file("external", "MODIS_TilesPolys.RData", package="MODIS"))

# save(MODIS_Products,file="~/MODIS_Products.RData") # in chase of changes
load(system.file("external", "MODIS_Products.RData", package="MODIS"))

load(system.file("external", "collections.RData", package="MODIS"))

## pitty that this does not work like that!
## lazy load gdal EPSG
#if (require(rgdal))
#{
#    EPSGinfo <- make_EPSG() # if rgdal, make it new!
#} else
#{
    # save(EPSGinfo,file="~/EPSGinfo.RData") # periodically safed manually by Admin (last up: October2014)
    load(system.file("external", "EPSGinfo.RData", package="MODIS"))
#}
###

# FTP information
# save(MODIS_FTPinfo,file="~/MODIS_FTPinfo.RData") # in chase of changes
load(system.file("external", "MODIS_FTPinfo.RData", package="MODIS")) 

# mrtOutDriver  <- c("raw binary","HDF-EOS","GeoTiff","HDF4Image","GTiff")

