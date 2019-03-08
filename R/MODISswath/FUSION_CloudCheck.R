###################################
#
# Select periode and area (exp. by: Shapefile - WGS84). Script gets MxD03,MxD35. Then determins CloudCover and writes to cvs file
# Skript needs MRTswath (swath2grid) tool in path
# Matteo Mattiuzzi IVFL 
# License GPLv 3+
#
###################################

######
library(raster)
library(RCurl)
library(PBSmapping)
library(audio)
######


run<- function(){

wrkdr <- "~/MODIS_Parameterfiles/MODIS_geolocated/" #
archive <- "~/MODIS_ARCHIVE/SWATH_data/"


archive <- path.expand(archive)
wrkdr <- path.expand(wrkdr)
setwd(wrkdr)

# die AOI einlesen 
# AOIpol <- importPolys(paste(wrkdr,"AOIset_Marchfeld.txt",sep=""))
AOIpol <- importShapefile(Job)

sy <- strsplit(startdate,"-")[[1]][1]
ey <- strsplit(enddate,"-")[[1]][1]
period <- sy:ey

### create FTP-Dir-string
HIR1  <- if(Platform == "both"){c("MOLT", "MOLA")}else{Platform}
HIR2  <- if(Platform == "both"){c("MOD", "MYD")}else{if(Platform == "TERRA"){"MOD"}else{if(Platform=="AQUA"){"MYD"}}}
HIR3  <- if(Platform == "both"){c("TERRA", "AQUA")}else{Platform}
ftps <- rep(NA,length(HIR1))
for (u in 1:length(HIR1)){
	ftps[u] <- paste("ftp://ladsweb.nascom.nasa.gov/geoMeta/", HIR3[u],"/",sep="")
}
####

for(z in 1:length(HIR1)){

if (file.exists(paste("summaries_",HIR2[z],"_",Job,".csv",sep=""))) {read.csv(paste("summaries_",HIR2[z],"_",Job,".csv",sep=""))}else {
  clResult<-list()
  s=0}

for (y in seq(along=period)){

getlist <- strsplit(getURL(paste(ftps[z],period[y],"/",sep=""),.opts=curlOptions(ftplistonly=TRUE)), if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]] 

items   <- unlist(lapply(getlist, function(x) strsplit(x,"\\.txt")[[1]][1]))
items   <- unlist(lapply(items, function(x) strsplit(x," ")[[1]][1]))
items   <- unlist(lapply(items, function(x) strsplit(x, paste(HIR2,"03_",sep=""))[[1]][2]))
datnum  <- unlist(lapply(items,function(x)as.numeric(paste(strsplit(x,"-")[[1]],sep="", collapse=""))))

sd  <- as.numeric(paste(strsplit(startdate,"-")[[1]],sep="",collapse=""))
ed  <- as.numeric(paste(strsplit(enddate,"-")[[1]],sep="",collapse=""))

us  <- datnum >= sd & datnum <= ed
filenames <- paste(HIR2[z],"03_",items,sep="")
filenames <- filenames[us]

#dd <- duplicated(filenames)
#i=0

for (i in 1:length(filenames)){ 

url <- paste(ftps[z],period[y],"/",filenames[i],".txt",sep="") # hard
cat("\n  #    #    #    #    #    #    #    #    #    #    #    #    #    #	\n  ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##\n #### #### #### #### #### #### #### #### #### #### #### #### #### ####\n#######################################################################\n\n",url,"\n\n#######################################################################\n #### #### #### #### #### #### #### #### #### #### #### #### #### ####\n  ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##\n  #    #    #    #    #    #    #    #    #    #    #    #    #    #\n")
flush.console()

rm(files)
	try(files <- read.table(url,sep=",",header=F),silent=T) # read.table error handling on missing data in geoMeta.txt file
if (exists("files")){
	msk1  <- files[,"V5"] == dayflag
	input <- files[msk1,]
	nData = nrow(input)

	PBS <- "PBSset.txt"
	filename = file(PBS, open="wt")
	write("PID POS X Y", filename)
	for (f in 1:nData){
	write(paste(f, 1, input[f,10], input[f,14], sep=" "), filename)
	write(paste(f, 2, input[f,11], input[f,15], sep=" "), filename)
	write(paste(f, 3, input[f,12], input[f,16], sep=" "), filename)
	write(paste(f, 4, input[f,13], input[f,17], sep=" "), filename)
	}
	close(filename)

	################################

	## die SWATH-Polygone 
	GEOpol <- importPolys("PBSset.txt")

	## Polygone verschneiden 
	intersect <- joinPolys( polysA=GEOpol, polysB=AOIpol,operation="INT" )
	if (length(intersect)!=0){
	
	## Just for visualising 
	# plotMap( intersect, col=5 )
	# addPolys( AOIpol, border=2, lty=8, density=0 )

	## die Fläche der AOI 
	area1 <- calcArea(AOIpol) 

	## die Schnittflächen AOI&GEO 
	areas = calcArea(intersect)

	## die MOD03 Filenamen rausschreiben 
	Files <- "Files.txt"
	filename = file(Files, open="wt")
	for (u in 1:dim(areas)[1]){
		write(paste(input[areas[u,1],1], areas[u,2]/area1[2]*100, sep=" "), filename )
	}
	close(filename)
	
	################################
	to35 <- read.table("Files.txt")
	# instersectProz <- to35[,2]# insert coverage limit here but it doesn't make sense
	to35 <- as.character(to35[,1])
	MOD03File <- to35
	to35 <- lapply(to35,function(x){strsplit(x,"\\.")[[1]]})

	for(l in seq(along=to35)){
		datum <- to35[[l]][2]
		time <-  to35[[l]][3]
		doy <- substring(datum,6,8)
		year <- substring(datum,2,5)

		###### download related GEOLOCATION file MxD03
		dir.create(paste(archive,HIR2[z],"03/",sep=""),showWarnings = FALSE) 		
		FTP03 <- paste("ftp://ladsweb.nascom.nasa.gov/allData/",as.numeric(Version),"/",HIR2[z],"03/",year,"/",doy,"/",sep="")
		if(!file.exists(paste(archive,HIR2[z],"03/",MOD03File[l],sep=""))){
			download.file(paste(FTP03,MOD03File[l],sep=""),destfile=paste(archive,HIR2[z],"03/",MOD03File[l],sep=""),mode="wb",method="wget",quiet=F,cacheOK=FALSE)
		}
		if(!file.exists(paste(archive,HIR2[z],"03/",MOD03File[l],sep=""))){
			cat( "Fehler ", FTP03, " ist nicht vorhanden!\n" )
			flush.console()
			stop()
		}
		
		outDir <- paste(wrkdr,HIR2[z],"35Grid/",Job,"/",sep="")
		dir.create(outDir,showWarnings = FALSE,recursive=T)
		
		MRT <- paste(getwd(),'/MRTkicker.prm',sep='')
		filename = file(MRT, open='wt')

		write(paste('INPUT_FILENAME = ',archive,HIR2[z],'03/',MOD03File[l],sep=''), filename)
		write(paste('GEOLOCATION_FILENAME = ',archive,HIR2[z],'03/',MOD03File[l],sep=''),filename)
		write(paste('INPUT_SDS_NAME = SensorZenith, 1',sep=''),filename)
		write(paste('OUTPUT_SPATIAL_SUBSET_TYPE = LAT_LONG',sep=''),filename)
		write(paste('OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) = ',min(AOIpol[,'X']),' ',max(AOIpol[,'Y']),sep=''),filename)
		write(paste('OUTPUT_SPACE_LOWER_RIGHT_CORNER (LONG LAT) = ',max(AOIpol[,'X']),' ',min(AOIpol[,'Y']),sep=''),filename)
		write(paste('OUTPUT_FILENAME = ',outDir,HIR2[z],'35G_',datum,'_',time,'.tif',sep=''),filename)
		write(paste('OUTPUT_FILE_FORMAT = GEOTIFF_FMT',sep=''),filename)
		write(paste('KERNEL_TYPE (CC/BI/NN) = ',RT,sep=''),filename)
		write(paste('OUTPUT_PROJECTION_NUMBER = ',Proj,sep=''),filename)
		write(paste('OUTPUT_PROJECTION_PARAMETER = 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0',sep=''),filename)
		write(paste('OUTPUT_PROJECTION_SPHERE = 8',sep=''),filename)
		write(paste('OUTPUT_PROJECTION_ZONE = ',Zone,sep=''),filename)
		close(filename)

		cmd = paste('swath2grid -pf=',MRT,sep='')		
		system(cmd)

		#system(cmd) # extract MxD35
		#unlink(MRT)
		### limit view angle here 
		zV <- raster(paste(outDir,HIR2[z],'35G_',datum,'_',time,'_SensorZenith.tif',sep=''))
		
		minmax <- range(zV[])
		
	if (abs(minmax[1]) < 30000) { # go on only if ZviewAnge is smaller than 30000 

		###### download MxD35 CLOUDMASK, to move behind zV
		dir.create(paste(archive,HIR2[z],"35",sep=""),showWarnings = FALSE)

		if (length(grep(dir(paste(archive,HIR2[z],"35",sep="")),pattern=paste(HIR2[z],"35_L2.",paste(to35[[l]][2:4],sep="",collapse="."),".*",sep=""),value=T))!=1){
			FTP35 <- paste("ftp://ladsweb.nascom.nasa.gov/allData/",as.numeric(Version),"/",HIR2[z],"35_L2/",year,"/",doy,"/",sep="")
			list35 <- strsplit(getURL(FTP35, .opts=curlOptions(ftplistonly=TRUE)), if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]]
			MOD35File <- grep(list35, pattern=paste(HIR2[z],"35_L2.",paste(to35[[l]][2:4],sep="",collapse="."),".*",sep=""),value=T)
			download.file(paste(FTP35,MOD35File,sep=""),destfile=paste(archive,HIR2[z],"35/",MOD35File,sep=""),mode="wb",method="wget",quiet=F,cacheOK=FALSE)
		}else{
			MOD35File <- grep(dir(paste(archive,HIR2[z],"35",sep="")),pattern=paste(HIR2[z],"35_L2.",paste(to35[[l]][2:4],sep="",collapse="."),".*",sep=""),value=T)
		}
		#if(!file.exists(paste(getwd(),"/",HIR2,"35HDF/",MOD35File,sep=""))){
		#}
		#download.file(paste(FTP35,MOD35File,sep=""),destfile=paste(getwd(),"/",HIR2,"35HDF/",MOD35File,sep=""),mode="wb",method="wget",quiet=T,cacheOK=FALSE)
		#######
	
		MRT <- paste(wrkdr,'/MRTkicker.prm',sep='')
		filename = file(MRT, open='wt')

		write(paste('INPUT_FILENAME = ',archive,HIR2[z],'35/',MOD35File,sep=''), filename)
		write(paste('GEOLOCATION_FILENAME = ',archive,HIR2[z],'03/',MOD03File[l],sep=''),filename)
		write(paste('INPUT_SDS_NAME = Cloud_Mask, 1, 0, 0, 0, 0, 0',sep=''),filename)
		write(paste('OUTPUT_SPATIAL_SUBSET_TYPE = LAT_LONG',sep=''),filename)
		write(paste('OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) = ',min(AOIpol[,'X']),' ',max(AOIpol[,'Y']),sep=''),filename)
		write(paste('OUTPUT_SPACE_LOWER_RIGHT_CORNER (LONG LAT) = ',max(AOIpol[,'X']),' ',min(AOIpol[,'Y']),sep=''),filename)
		write(paste('OUTPUT_FILENAME = ',outDir,HIR2[z],'35G_',datum,'_',time,'.tif',sep=''),filename)
		write(paste('OUTPUT_FILE_FORMAT = GEOTIFF_FMT',sep=''),filename)
		write(paste('KERNEL_TYPE (CC/BI/NN) = ',RT,sep=''),filename)
		write(paste('OUTPUT_PROJECTION_NUMBER = ',Proj,sep=''),filename)
		write(paste('OUTPUT_PROJECTION_PARAMETER = 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0',sep=''),filename)
		write(paste('OUTPUT_PROJECTION_SPHERE = 8',sep=''),filename)
		write(paste('OUTPUT_PROJECTION_ZONE = ',Zone,sep=''),filename)
		close(filename)

		system(paste('swath2grid -pf=',MRT,sep='')) # extract MxD35
		#unlink(MRT)

		b0   <- raster(paste(outDir,HIR2[z],'35G_',datum,'_',time,'_Cloud_Mask_b0.tif',sep=''))
		NAvalue(b0)<--1

dat <- calc(b0,function(x) bitAnd(bitShiftR(x,1),3),filename=paste(outDir,HIR2[z],'35G_',datum,'_',time,'_ExtrCloudM.tif',sep=''),overwrite=T)
		
		unlink(paste(outDir,HIR2[z],'35G_',datum,'_',time,'_Cloud_Mask_b0.tif',sep='')) # not more needed
		
		nC     <- ncell(b0)
		free   <- sum(dat[] == 3)
		pfree  <- sum(dat[] == 2)
		pcloud <- sum(dat[] == 1)
		cloud  <- sum(dat[] == 0)
		NAs    <- sum(is.na(dat[]))
		s=s+1
		clResult[[s]]<- c(MOD03File[l],free,pfree,pcloud,cloud,NAs,nC,minmax) 

		#cat("\n", free, "\n", pfree, "\n", pcloud, "\n", cloud, "\n", NAs, "\n" );
		#flush.console()

		#stop()

		#if (max(b0[])!=0){
		#values(b0) <- dat
		#NAvalue(b0)<- -1
		#b0   <- writeRaster(b0,paste(outDir,HIR2,'35G_',datum,'_',time,'bitEx','.bil',sep=''),overwrite=T,datatype="INT1S")#
		#}

		# unlink(paste(getwd(),'/',HIR2,'03/',MOD03File[l],sep=''))
		# unlink(paste(getwd(),'/',HIR2,'35HDF/',MOD35File,sep=''))

		x <- matrix(unlist(clResult),ncol=9,byrow=T)
		#colnames(x)<- c("MOD03Filename","Free","probFree","probCloudy","Cloudy","NAs","nCell","MaxViewAngle","MinViewAngle")
		write.csv2(x,paste("summaries_",HIR2[z],"_",Job,".csv",sep=""))

	}   # if min zV>3000 close
	} # in day process close [l]
} # if intersect == 0
} # if no files in filename[i] (geoMeta.txt-file)
} # in period (startdate to enddate) process close [i]
colnames(x)<- c("MOD03Filename","Free","probFree","probCloudy","Cloudy","NAs","nCell","MaxViewAngle","MinViewAngle")
write.csv2(x,paste("summaries_",HIR2[z],"_",Job,".csv",sep=""))
} # y , period sy:ey 
} # platform MOD/MYD switch close [z]	  

} # end Function "run()"

