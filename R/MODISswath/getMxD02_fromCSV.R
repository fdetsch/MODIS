

Platform="both"
Job = "UKRAINA"


run <- function(){
### create FTP-Dir-string
HIR1  <- if(Platform == "both"){c("MOLT", "MOLA")}else{Platform}
HIR2  <- if(Platform == "both"){c("MOD", "MYD")}else{if(Platform == "TERRA"){"MOD"}else{if(Platform=="AQUA"){"MYD"}}}
HIR3  <- if(Platform == "both"){c("TERRA", "AQUA")}else{Platform}


for(z in 1:length(HIR1)){

data <- read.csv2(paste("summaries_",HIR2[z],"_",Job,".csv",sep=""))

for (i in 1:nrow(data)){

MxD03 <- as.character(data[i,2])
basenam <- strsplit(MxD03,"\\.")[[1]][2:4]
PVer  <- as.numeric(strsplit(MxD03,"\\.")[[1]][4])
Year  <- substr(strsplit(MxD03,"\\.")[[1]][2],2,5)
Doy   <- substr(strsplit(MxD03,"\\.")[[1]][2],6,8) 

ftp <- paste("ftp://ladsweb.nascom.nasa.gov/allData/",PVer,"/",sep="")

if (Producttype == "ALL"){ 
PType <- c("1KM","HKM","QKM")

} else {stop("only 'ALL' is supported for 'Producttype', just call me if you need changes")}

#
prenam <- paste(HIR2[z],"02",PType,sep="")
ftpMxD02 <- paste(ftp,prenam,"/",Year,"/",Doy,"/",sep="")

pattern <- paste(prenam,basenam[1],basenam[2],basenam[3],sep=".")
pattern <- paste(pattern,".*.hdf",sep="")

MxD02Av <- list()
for(q in  1:length(pattern)){
MxD02Av[[q]] <- grep(dir(paste(archive,HIR2[z],"02",sep="")),pattern=pattern[q],value=T)
}

if (sum(MxD02Av!="character(0)")==0) { # if no PType is available on archive, get the file pattern from ftp (looks only for ftpMxD02[1]!)
onFTP <- strsplit(getURL(ftpMxD02[1],.opts=curlOptions(ftplistonly=TRUE)), if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]] 
MxD02 <- grep(onFTP,pattern=pattern[1],value=T)
#MxD02Av <- as.list(rep("character(0)",length(PType)))
if (length(MxD02)==0){stop(paste('file with pattern: ',pattern[1],' not found in: ',ftpMxD02,sep=""))} 
} else {
MxD02     <- unlist(MxD02Av)[MxD02Av!="character(0)"]
MxD02     <- MxD02[1]
}

backpart <- paste(strsplit(MxD02,"\\.")[[1]][-1],collapse=".")
MxD02    <- paste(prenam,backpart,sep=".")

if (sum(MxD02Av=="character(0)")!=0){
for (x in which(MxD02Av=="character(0)")){
download.file(paste(ftpMxD02[x],MxD02[x],sep=""),destfile=paste(archive,HIR2[z],"02/",MxD02[x],sep=""),mode="wb",method="wget",quiet=F,cacheOK=FALSE)
wait(0.5)
}
}

} # end i (nrow(data))
} # end z (HIR1)
} # end run()






