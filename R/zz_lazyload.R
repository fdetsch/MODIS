# lazy loading
##########################################

## check if package is installed (<-> do not use installed.packages() here, see 
## documentation)
avl = length(find.package("MODIS", quiet = TRUE) > 0)

ofl = ifelse(!avl
             , file.path(getwd(), "inst/external")
             , system.file("external", package = "MODIS"))
ofl = gsub("/R/inst/external", "/inst/external", ofl)

tiletable <- read.table(file.path(ofl, "tiletable.txt"), header = TRUE)

# save(tileNames,file="~/tileNames.RData") # in case of changes
load(file.path(ofl, "tileNames.RData"))

# load MODIS Tiles (sr)
load(file.path(ofl, "MODIS_TilesPolys.RData"))

# save(MODIS_Products,file="~/MODIS_Products.RData") # in case of changes
load(file.path(ofl, "MODIS_Products.RData"))

load(file.path(ofl, "collections.RData"))

# FTP information
# save(MODIS_FTPinfo,file="~/MODIS_FTPinfo.RData") # in case of changes
load(file.path(ofl, "MODIS_FTPinfo.RData")) 

# mrtOutDriver  <- c("raw binary","HDF-EOS","GeoTiff","HDF4Image","GTiff")

