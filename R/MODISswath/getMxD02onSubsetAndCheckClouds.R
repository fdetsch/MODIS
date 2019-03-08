###################################
#
# Select periode and area (exp. by: Shapefile - WGS84). Script gets MxD03,MxD35. Then determins CloudCover and writes to cvs file
# Skript needs MRTswath (swath2grid) tool in path
# Matteo Mattiuzzi IVFL 
# License GPLv 3+
#
###################################
# todo: if a 180/-180 crossing image is detected, do not further proceed with it (eventually with calc area of the image from "geoMeta")
# sesume Jobs (row49)



run<- function(){

######

require(MODIS)
require(PBSmapping)
require(maptools)
require(raster)
gpclibPermit()

######

wrkdr   <- "/home/arc/Desktop/MATTEOtemp"
archive <- "~/MODIS_ARC/SWATH_data/"
#archive <-"~/.gvfs/modis_arc on ivfl-arc/SWATH_data/"

startdate = "2010-01-01"
enddate = "2010-12-31" 
Platform = "TERRA" 
dayflag = "D" 
Version = "005" 
AOIFile = "/home/arc/Desktop/MATTEOtemp/AOIset_New.txt" # in wrkdr or provied full path
Job = "UKRAINA"
RT = "NN" 
Proj = "UTM" 
Zone = 35

maxAngle = 50 

archive <- path.expand(archive)
wrkdr   <- path.expand(wrkdr)

# die AOI einlesen
AOIpol<-importPolys(AOIFile,projection="UTM",zone=35)
#AOIpol <- importShapefile("/home/arc/Desktop/MATTEOtemp/AOIset_New.txt",projection="LL")

sy <- strsplit(startdate,"-")[[1]][1]
ey <- strsplit(enddate,"-")[[1]][1]
period <- sy:ey

if(sy==ey) {
periodname <- paste("_",sy,sep="")
} else {
periodname <- paste("_",sy,"_",ey,sep="")
}


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

subwrkdr <- file.path(paste(wrkdr,"/",HIR2[z],"_",Job,periodname,sep=""))
dir.create(subwrkdr)
setwd(subwrkdr)

# NOT READY! if file exixts, proceed and do not start with the Job from the beginning. Probable not ok here, to move inside y
if (FALSE) { #if (file.exists(paste("summaries_",HIR2[z],"_",Job,".csv",sep=""))) {
		x <- read.csv(paste("summaries_",HIR2[z],"_",Job,periodname,".csv",sep=""))
		s=nrow(x)
		clResult <- as.list(x)
	}else{ 
  	clResult <- list()
  	s=0
  }

for (y in seq(along=period)){
require(RCurl)

getlist <- strsplit(getURL(paste(ftps[z],period[y],"/",sep=""),.opts=curlOptions(ftplistonly=TRUE)), if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]] 
Sys.sleep(0.5)
items   <- unlist(lapply(getlist, function(x) strsplit(x,"\\.txt")[[1]][1]))
items   <- unlist(lapply(items, function(x) strsplit(x," ")[[1]][1]))
items   <- unlist(lapply(items, function(x) strsplit(x, paste(HIR2[z],"03_",sep=""))[[1]][2]))
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
cat("\n  #    #    #    #    #    #    #    #    #    #    #    #    #    #	\n  ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##\n #### #### #### #### #### #### #### #### #### #### #### #### #### ####\n#######################################################################\n\n",url,"\n'i' is:",i,"\n'y' is:",y,"\n'z' is:",z,"\n\n#######################################################################\n #### #### #### #### #### #### #### #### #### #### #### #### #### ####\n  ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##\n  #    #    #    #    #    #    #    #    #    #    #    #    #    #\n\n")
flush.console()

if (exists("files")) {rm(files)}
	try(files <- read.table(url,sep=",",header=F),silent=T) # read.table error handling on missing data in geoMeta.txt file
if (exists("files")){
	msk1  <- files[,"V5"] == dayflag
	input <- files[msk1,]
	nData = nrow(input)

	PBS <- paste(subwrkdr,"/PBSset.txt",sep="")
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
	GEOpol <- importPolys(PBS,projection="LL")
	
	# if swath is crossing 180/-180, selected by area! (aproximative but should work)
	areas <- list()
	for(u in unique(GEOpol$PID)) {
	areas[[u]]<- calcArea(GEOpol[GEOpol$PID==u,])$area
	}
	areas <- unlist(areas)
	areas <- rep(areas,each=4) # this is a little hard coded, each=4 works only with 4 points inside a ploygon (is mainly right but not 100% save!
	usePID <- areas < 5500000 # !!!!!!! Area of MOD02 file, if it is much bigger than the problem of crossing -180/180 is present, to check!
	rm(areas)
	GEOpol <- GEOpol[usePID,]
	
	## Polygone verschneiden 
	intersect <- joinPolys(polysA=GEOpol, polysB=AOIpol,operation="INT" )
	if (length(intersect)!=0){
	
	## Just for visualising 
	# plotMap( intersect, col=5 )
	# addPolys( AOIpol, border=2, lty=8, density=0 )

	## die Flaeche des AOIs
	area1 <- calcArea(AOIpol) 

	## die Schnittflaechen AOI&GEO 
	areas <- list()
	hu=0
	for(u in unique(intersect$PID)) {
	hu=hu+1
	areas[[hu]]<- calcArea(intersect[intersect$PID==u,])#$area
	}
	areas <- matrix(unlist(areas),ncol=2,byrow=T)
	#areas = calcArea(intersect)

	## die MOD03 Filenamen rausschreiben 
	Files <- paste(subwrkdr,"/Files.txt",sep="")
	filename = file(Files, open="wt")
	for (u in 1:nrow(areas)){
		write(paste(input[areas[u,1],1], areas[u,2]/area1[2]*100, sep=" "), filename )# areas[u,1] is the PID of the uesd polygon! expecting that it is allienated with the masked geoMeta file number! It seams to work, but I do not trust 100% on that!
	}
	close(filename)
	
	################################
	to35 <- read.table(Files)
	to35 <- as.character(to35[,1])
	MOD03File <- to35
	to35 <- lapply(to35,function(x){strsplit(x,"\\.")[[1]]})

	for(l in seq(along=to35)){
		datum <- to35[[l]][2]
		time <-  to35[[l]][3]
		doy <- substring(datum,6,8)
		year <- substring(datum,2,5)

		# generate a ftp like path on local archive
		dir03 <- paste(archive,HIR2[z],"03/",year,"/",doy,"/",sep="")
		dir.create(dir03,showWarnings=FALSE,recursive=TRUE)
		###### download related GEOLOCATION file MxD03

		FTP03 <- paste("ftp://ladsweb.nascom.nasa.gov/allData/",as.numeric(Version),"/",HIR2[z],"03/",year,"/",doy,"/",sep="")
		
		if(!file.exists(paste(dir03,MOD03File[l],sep=""))){
		
			g=1
			while(g <= 1000) {
				try(hdf <- download.file(paste(FTP03,MOD03File[l],sep=""),destfile=paste(dir03,MOD03File[l],sep=""),
				mode="wb",method="auto",quiet=FALSE,cacheOK=FALSE),silent=TRUE)
			
				if(hdf==0) {break}
				g=g+1
				Sys.sleep(0.3)
			}
			if(hdf==1){
				cat( "Fehler ",FTP03,MOD03File[l]," ist nicht vorhanden!\n",sep="")
				flush.console()
			}
		}
			
		outDir <- paste(wrkdr,"/",HIR2[z],"35Grid/",Job,periodname,"/",sep="")
		dir.create(outDir,showWarnings = FALSE,recursive=T)
		
		MRT <- paste(subwrkdr,'/MRTkicker.prm',sep='')
		filename = file(MRT, open='wt')

		write(paste('INPUT_FILENAME = ',dir03,MOD03File[l],sep=''), filename)
		write(paste('GEOLOCATION_FILENAME = ',dir03,MOD03File[l],sep=''),filename)
		write(paste('INPUT_SDS_NAME = SensorZenith',sep=''),filename)
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
		maxA <- max(abs(minmax))
		
	if (maxA < (maxAngle*100)) { # go on only if ZviewAnge is smaller than 30000 

		###### download MxD35 CLOUDMASK
		dir35 <- paste(archive,HIR2[z],"35/",year,"/",doy,"/",sep="")
		dir.create(dir35,showWarnings = FALSE,recursive=TRUE)

		if (length(list.files(dir35,pattern=paste(HIR2[z],"35_L2.",paste(to35[[l]][2:4],sep="",collapse="."),".*.hdf$",sep="")))!=1){
			FTP35 <- paste("ftp://ladsweb.nascom.nasa.gov/allData/",as.numeric(Version),"/",HIR2[z],"35_L2/",year,"/",doy,"/",sep="")
			list35 <- strsplit(getURL(FTP35, .opts=curlOptions(ftplistonly=TRUE)), if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]]
			Sys.sleep(0.5)
			MOD35File <- grep(list35,pattern=paste(HIR2[z],"35_L2.",paste(to35[[l]][2:4],sep="",collapse="."),".*.hdf$",sep=""),value=TRUE)
			
				g=1
				while(g <= 1000) {
					try(hdf <- download.file(paste(FTP35,MOD35File,sep=""),destfile=paste(dir35,MOD35File,sep=""),
					mode="wb",method="auto",quiet=FALSE,cacheOK=FALSE),silent=TRUE)
			
					if(hdf==0) {break}
					g=g+1
					Sys.sleep(0.3)
				}
				
				if(hdf==1){
					cat( "Fehler ", FTP35,MOD35File, " ist nicht vorhanden!\n",sep="" )
					flush.console()
				}
				
		} else {
			MOD35File <- list.files(dir35,pattern=paste(HIR2[z],"35_L2.",paste(to35[[l]][2:4],sep="",collapse="."),".*.hdf$",sep=""))
		}
	
		#MRT <- paste(wrkdr,'/MRTkicker.prm',sep='')
		filename = file(MRT, open='wt')

		write(paste('INPUT_FILENAME = ',dir35,MOD35File,sep=''), filename)
		write(paste('GEOLOCATION_FILENAME = ',dir03,MOD03File[l],sep=''),filename)
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
		unlink(MRT)

		b0          <- raster(paste(outDir,HIR2[z],'35G_',datum,'_',time,'_Cloud_Mask_b0.tif',sep=''))
		NAvalue(b0) <- -1

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

		#if (max(b0[])!=0){
		#values(b0) <- dat
		#NAvalue(b0)<- -1
		#b0   <- writeRaster(b0,paste(outDir,HIR2,'35G_',datum,'_',time,'bitEx','.bil',sep=''),overwrite=T,datatype="INT1S")#
		#}

		x <- matrix(unlist(clResult),ncol=9,byrow=T)
		write.csv2(x,paste(wrkdr,"/summaries_",HIR2[z],"_",Job,periodname,".csv",sep=""))

	} else {
	cat("View angle criteria not reached.\n")
	} # if min zV>maxAngle close
	} # in day process close [l]
} # if intersect == 0
} # if no files in filename[i] (geoMeta.txt-file)
} # in period (startdate to enddate) process close [i]
colnames(x)<- c("MOD03Filename","Free","probFree","probCloudy","Cloudy","NAs","nCell","MinViewAngle","MaxViewAngle")
write.csv2(x,paste(wrkdr,"/summaries_",HIR2[z],"_",Job,periodname,".csv",sep=""))
} # y , period sy:ey 
} # platform MOD/MYD switch close [z]	  

} # end Function "run()"

run()
traceback()

