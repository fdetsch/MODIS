# Purose:
# get into csv table (output of "Fusion_CloudCheck.R" and resample MxD02xkm to grid
#
######
library(raster)
library(RCurl)
library(PBSmapping)
######
useExt <- T # (if exist) use Extension file: "EXTENT_1ULlat_2ULlon_3LRlat_4LRlon.txt"
Proj  <- "UTM"# proj of output
Zone  <- "33" # 
RT    <- "CC"  # resampletype NN, CC, BI

###################################
### Parameter input
######################
# personal setting:
if (.Platform$OS.type == "unix"){
#HEG   <- "/usr/share/HEG/bin/"
wrkdr <- "/home/matteo/Desktop/FUSIO_CloudCheck/MODIS_clF_2009/"
} else {
#HEG   <- "c:\\Programme\\heg\\HEG_Win\\bin\\"
wrkdr <- "D:\\Fusion\\"
}
setwd(wrkdr)
######
AOIpol <- importPolys("/home/matteo/Desktop/FUSIO_CloudCheck/AOIset_Marchfeld.txt")
r <- raster("/home/matteo/Desktop/NDVI_2009_stack.img")
allEx<- extent(r)

av <- read.csv2("MOD35_statsTable.csv")

for(i in 33:53){ # 1:nrows(av) # for all

todo <- av[i,]

MxD03 <- as.character(todo[2][[1]])
subM03 <- strsplit(MxD03,"\\.")[[1]]

plf     <-  substr(subM03[1],2,2)
if (plf == "Y") {plf <- "MYD"} else {plf <- "MOD"}
rootn   <- paste(subM03[2:4],sep="",collapse=".")
splroot <- strsplit(rootn,"\\.")[[1]]
vers    <- splroot[3]
year    <- substr(splroot[1],2,5)
doy     <- substr(splroot[1],6,8)

MxD03Dr     <- paste(plf,"03/",sep="")
MxD02Dr     <- "MODIS_02_hdfs/"
MxD02Nam    <- grep(dir(paste(getwd(),'/',MxD02Dr,sep="")),pattern=paste(plf,"02QKM.",rootn,".*",sep=""),value=T)
strMxD02Nam <- strsplit(MxD02Nam,"\\.")[[1]]

#######
outDir      <- paste(getwd(),"/",plf,"02Grid/",sep="")
dir.create(outDir,showWarnings = FALSE)

MRT <- paste(getwd(),'/MRTkicker.prm',sep='')
filename = file(MRT, open='wt')

write(paste('INPUT_FILENAME = ',getwd(),'/',MxD02Dr,MxD02Nam,sep=''), filename)
write(paste('GEOLOCATION_FILENAME = ',getwd(),'/',MxD03Dr,MxD03,sep=''),filename)
write(paste('INPUT_SDS_NAME = EV_250_RefSB, 0,1',sep=''),filename)
write(paste('OUTPUT_SPATIAL_SUBSET_TYPE = LAT_LONG',sep=''),filename)
write(paste('OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) = ',min(AOIpol[,'X']),' ',max(AOIpol[,'Y']),sep=''),filename)
write(paste('OUTPUT_SPACE_LOWER_RIGHT_CORNER (LONG LAT) = ',max(AOIpol[,'X']),' ',min(AOIpol[,'Y']),sep=''),filename)
write(paste('OUTPUT_FILENAME = ',outDir,plf,'02G_',rootn,'_',RT,'.tif',sep=''),filename)
write(paste('OUTPUT_FILE_FORMAT = GEOTIFF_FMT',sep=''),filename)
write(paste('KERNEL_TYPE (CC/BI/NN) = ',RT,sep=''),filename)
write(paste('OUTPUT_PROJECTION_NUMBER = ',Proj,sep=''),filename)
write(paste('OUTPUT_PROJECTION_PARAMETER = 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0',sep=''),filename)
write(paste('OUTPUT_PROJECTION_SPHERE = 8',sep=''),filename)
write(paste('OUTPUT_PROJECTION_ZONE = ',Zone,sep=''),filename)
close(filename)

system(paste('swath2grid -pf=',MRT,sep='')) # extract MxD35
}


#system(paste('read_sds_attributes ',getwd(),'/',MxD02Dr,MxD02Nam,sep=''))

