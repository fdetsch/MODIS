
######
library(raster)
library(RCurl)
library(PBSmapping)
######
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
# Parameters

# max view angle

# max clouds

# FTP string



av <- read.csv2("final2.csv")

for(i in 1:nrows(av){#23:37){ #  # for all

todo <- av[i,]

MxD03 <- as.character(todo[2][[1]])
subM03 <- strsplit(MxD03,"\\.")[[1]]

plf <-  substr(subM03[1],2,2)
if (plf == "Y") {plf <- "MYD"} else {plf <- "MOD"}
rootn <- paste(subM03[2:4],sep="",collapse=".")
splroot <- strsplit(rootn,"\\.")[[1]]
vers <- splroot[3]
year <- substr(splroot[1],2,5)
doy <- substr(splroot[1],6,8)

# FTPString
drn <- paste("MODIS_02_hdfs",sep="")
if (.Platform$OS.type == "unix") {dir.create(drn,showWarnings=F, mode = "777")} else {dir.create(drn,showWarnings=F)}
FTP02a <- "ftp://ladsweb.nascom.nasa.gov/allData/"
FTP02b <- paste(FTP02a,as.numeric(vers),"/",sep="")

# getFiles (1km,hkm,qkm)
FTP1KM <- paste(FTP02b,plf,"021KM/",year,"/",doy,"/",sep="")
getlist <- strsplit(getURL(FTP1KM,.opts=curlOptions(ftplistonly=TRUE)), if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]] 
fil1 <- grep(getlist,pattern=paste(plf,"021KM.",rootn,".*",sep=""),value=T)
download.file(paste(FTP1KM,fil1,sep=""),destfile=paste(getwd(),"/",drn,"/",fil1,sep=""),mode="wb",method="wget",quiet=T,cacheOK=FALSE)

filb <- paste(strsplit(fil1,"\\.")[[1]][-1],sep="",collapse=".")

FTPHKM <- paste(FTP02b,plf,"02HKM/",year,"/",doy,"/",sep="")
filH <- paste(plf,"02HKM.",filb,sep="")
download.file(paste(FTPHKM,filH,sep=""),destfile=paste(getwd(),"/",drn,"/",filH,sep=""),mode="wb",method="wget",quiet=T,cacheOK=FALSE)

FTPQKM <- paste(FTP02b,plf,"02QKM/",year,"/",doy,"/",sep="")
filQ <- paste(plf,"02QKM.",filb,sep="")
download.file(paste(FTPQKM,filQ,sep=""),destfile=paste(getwd(),"/",drn,"/",filQ,sep=""),mode="wb",method="wget",quiet=T,cacheOK=FALSE)
} # end i

