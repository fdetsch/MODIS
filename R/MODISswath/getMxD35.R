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
#gpclibPermit()


getSwath <- function(product="M.D02",extent=NULL, job=NULL,begin=NULL,end=NULL,dayflag="D",RT="NN",Proj="UTM",zone="", wrkdr = "~/Desktop/r_tests", archive = "~/.gvfs/SFTP for arc on 141.244.148.20/home/arc/MODIS_ARC/SWATH_data")
{

    require(MODIS)
    archive <- path.expand(archive)
    wrkdr   <- path.expand(wrkdr)
    setwd(wrkdr)
    
    # EXTENT (generation of the area of interest)
    ex <- strsplit(extent,"\\.")[[1]]
    if (ex[length(ex)]=="txt")
    {

        AOI <- extent  

    } else if ((ex[length(ex)]=="shp"))
    {

        AOI <- shapefile(extent)

    } else 
    {

        a <- getTile(extent=extent)
 
        AOI <- paste(wrkdr,"/Area_",job,".txt",sep="")
        filename = file(AOI, open="wt")
        write("PID POS X Y", filename)
        write(paste(1, 1, a$extent$xmin, a$extent$ymax, sep=" "), filename)
        write(paste(1, 2, a$extent$xmax, a$extent$ymax, sep=" "), filename)
        write(paste(1, 3, a$extent$xmax, a$extent$ymin, sep=" "), filename)
        write(paste(1, 4, a$extent$xmin, a$extent$ymin, sep=" "), filename)
        close(filename)
  
    }
            
    AOIpol <- importPolys(AOI)
    ####################################

    dates  <- transDate(begin=begin,end=end)
    period <- format(dates$begin,"%Y"):format(dates$end,"%Y")
    
    product <- getProduct(product,quiet=TRUE)
    
    ### create FTP-Dir-string
    HIR1  <- unique(product$PF1)
    HIR2  <- unique(product$PF2)
    HIR3  <- toupper(unique(product$PLATFORM))
    ftps <- rep(NA,length(HIR1))
    for (u in 1:length(HIR1))
    {
        ftps[u] <- paste("ftp://ladsweb.nascom.nasa.gov/geoMeta/", HIR3[u],"/",sep="")
    }
    ####
    
    for(z in seq_along(HIR1))
    {
    
        if (file.exists(paste("summaries_",HIR2[z],"_",job,".csv",sep=""))) 
        { # resumes a job if already parts are done!
            clResult <- read.csv2(paste("summaries_",HIR2[z],"_",job,".csv",sep=""))[,-1]
            s <- nrow(clResult)
            clResult <- as.list(clResult)
        } else 
        {
            clResult <- list()
            s=0
        }
    
        for (y in seq_along(period))
        {
            getlist <- filesUrl(paste(ftps[z],period[y],"/",sep="")) 
            items   <- unlist(lapply(getlist, function(x) strsplit(x,"\\.txt")[[1]][1]))
            items   <- unlist(lapply(items, function(x) strsplit(x," ")[[1]][1]))
            items   <- as.Date(unlist(lapply(items, function(x) strsplit(x, paste(HIR2,"03_",sep=""))[[1]][2])))
            
            us  <- items >= dates$begin & items <= dates$end
            
            #filenames <- paste(HIR2[z],"03_",items,sep="")
            filenames <- getlist[us]
    
            #dd <- duplicated(filenames)
            #i=0
    
            for (i in seq_along(filenames))
            { 
                url <- paste(ftps[z],period[y],"/",filenames[i],sep="")
                cat("\n  #    #    #    #    #    #    #    #    #    #    #    #    #    #    \n  ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##\n #### #### #### #### #### #### #### #### #### #### #### #### #### ####\n#######################################################################\n\n",url,"\n\n#######################################################################\n #### #### #### #### #### #### #### #### #### #### #### #### #### ####\n  ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##   ##\n  #    #    #    #    #    #    #    #    #    #    #    #    #    #\n\n")
                flush.console()

                try(rm(files))
                try(files <- read.table(url,sep=",",header=FALSE,comment.char = "#"),silent=TRUE) # read.table error handling on missing data in geoMeta.txt file
                if (exists("files"))
                {
                    colnames(files) <- c("GranuleID", "StartDateTime", "ArchiveSet", "OrbitNumber", "DayNightFlag", "EastBoundingCoord", "NorthBoundingCoord", "SouthBoundingCoord", "WestBoundingCoord", "GRingLongitude1", "GRingLongitude2", "GRingLongitude3", "GRingLongitude4", "GRingLatitude1", "GRingLatitude2", "GRingLatitude3", "GRingLatitude4")
                    if (toupper(dayflag) %in% c("D","N"))
                    {
                        msk  <- files[,"DayNightFlag"] == toupper(dayflag)
                        input <- files[msk,]
                    } else
                    {
                        input <- files
                    }
                    
                    nData = nrow(input)
                    po <- list()
                    g=0
                    msk <- rep(FALSE,nData)
                    for (f in 1:nData)
                    {                    
                        x <- Polygon(cbind(
                              c(input[f,"GRingLongitude1"], input[f,"GRingLongitude2"], input[f,"GRingLongitude3"], input[f,"GRingLongitude4"], input[f,"GRingLongitude1"]), 
                              c(input[f,"GRingLatitude1"], input[f,"GRingLatitude2"], input[f,"GRingLatitude3"], input[f,"GRingLatitude4"], input[f,"GRingLatitude1"])),
                              hole=FALSE)
                        # Try to ignore warped polygons      
                        if (x@area < 2000)
                        {
                            msk[f] <- TRUE
                            g = g+1
                            po[[g]] <- x    
                        }
                    }
                    SpatialPolygonsDataFrame(po, data=input[msk,1])
                    
                    pos  <- list(Polygons(po,"selection"))
                    spos <- SpatialPolygons(pos,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
                    
                    
                    input <- input[msk,1]
                    
                    plot(spos %over% AOI)
                    
                    rNum <- round(runif(1,1,100000),0)
                    PBS <- paste("PBSset",rNum,".txt",sep="")
                    filename = file(PBS, open="wt")
                    write("PID POS X Y", filename)
                    
                    for (f in 1:nData)
                    {
                        write(paste(f, 1, input[f,10], input[f,14], sep=" "), filename)
                        write(paste(f, 2, input[f,11], input[f,15], sep=" "), filename)
                        write(paste(f, 3, input[f,12], input[f,16], sep=" "), filename)
                        write(paste(f, 4, input[f,13], input[f,17], sep=" "), filename)
                    }
                    close(filename)
    
                ################################
        
                # PBS method
                ## die SWATH-Polygone 
                GEOpol <- importPolys(PBS)
                ## Polygone verschneiden 
                intersect <- joinPolys( polysA=GEOpol, polysB=AOIpol,operation="INT")

                if (length(intersect)!=0)
                {
                
                    ## Just for visualising 
                    # plotMap( intersect, col=5 )
                    # addPolys( AOIpol, border=2, lty=8, density=0 )
                
                    ## die Fläche der AOI 
                    area1 <- calcArea(AOIpol) 
                
                    ## die Schnittflächen AOI&GEO 
                    areas = calcArea(intersect)
                
                    ## die MOD03 Filenamen rausschreiben 
                    Files    <- paste("Files",rNum,".txt",sep="")
                    filename <- file(Files, open="wt")
                    for (u in 1:dim(areas)[1])
                    {
                        write(paste(input[areas[u,1],1], areas[u,2]/area1[2]*100, sep=" "), filename)
                    }
                    close(filename)
                    
                    unlink(PBS)
                    ################################
                    to35 <- read.table(Files)
                    # instersectProz <- to35[,2] # insert coverage limit here but it doesn't make sense
                    to35 <- as.character(to35[,1])
                    #MOD03File <- to35
                    to35 <- lapply(to35,function(x){strsplit(x,"\\.")[[1]]})
                
                    for(l in seq(along=to35))
                    {
                        datum <- to35[[l]][2]
                        time  <- to35[[l]][3]
                        doy   <- substring(datum,6,8)
                        year  <- substring(datum,2,5)
                
                        ###### download MxD35 CLOUDMASK, to move behind zV
                
                        destdir35 <- paste(archive,"/",HIR2[z],"35/",year,"/",doy,sep="")
                        dir.create(destdir35,recursive=TRUE,showWarnings = FALSE)
                        
                        MOD35File <- list.files(path=destdir35,pattern=paste("^",HIR2[z],"35_L2.",paste(to35[[l]][2:4],sep="",collapse="."),".*.hdf$",sep=""),recursive=TRUE,full.names=TRUE)
                        
                        if (length(MOD35File)!=1)
                        {
                            if (!exists("list35"))
                            {
                                FTP35  <- paste("ftp://ladsweb.nascom.nasa.gov/allData/",as.numeric(5),"/",HIR2[z],"35_L2/",year,"/",doy,"/",sep="")
                                list35 <- getURL(FTP35)
                                list35 <- strsplit(list35, if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]]
                                list35 <- unlist(lapply(strsplit(list35," "),function(x){x[length(x)]}))
                            }
                            MOD35File <- grep(list35, pattern=paste(HIR2[z],"35_L2.",paste(to35[[l]][2:4],sep="",collapse="."),".*",sep=""),value=T)
                            download.file(paste(FTP35,MOD35File,sep=""),destfile=paste(destdir35,"/",MOD35File,sep=""),mode="wb",method="wget",quiet=FALSE,cacheOK=FALSE)
                            MOD35File <- list.files(path=destdir35,pattern=paste("^",MOD35File,"$",sep=""),full.names=TRUE)
                        }
                
                        ###### download MxD03
                
                        destdir03 <- paste(archive,"/",HIR2[z],"03/",year,"/",doy,sep="")
                        dir.create(destdir03,recursive=TRUE,showWarnings = FALSE)
                        
                        MOD03File <- list.files(path=destdir03,pattern=paste(HIR2[z],"03.",paste(to35[[l]][2:4],sep="",collapse="."),".*.hdf$",sep=""),full.names=TRUE)
                        
                        if (length(MOD03File)!=1)
                        {
                            if (!exists("list03"))
                            {
                                FTP03  <- paste("ftp://ladsweb.nascom.nasa.gov/allData/",as.numeric(5),"/",HIR2[z],"03/",year,"/",doy,"/",sep="")
                                list03 <- getURL(FTP03)
                                list03 <- strsplit(list03, if(.Platform$OS.type=="unix"){"\n"} else{"\r\n"})[[1]]
                                list03 <- unlist(lapply(strsplit(list03," "),function(x){x[length(x)]}))
                            }
                            MOD03File <- grep(list03, pattern=paste(HIR2[z],"03.",paste(to35[[l]][2:4],sep="",collapse="."),".*",sep=""),value=TRUE)
                            download.file(paste(FTP03,MOD03File,sep=""),destfile=paste(destdir03,"/",MOD03File,sep=""),mode="wb",method="wget",quiet=FALSE,cacheOK=FALSE)
                            MOD03File <- list.files(path=destdir03,pattern=paste("^",MOD03File,"$",sep=""),full.names=TRUE)
                        }
                        
                        outDir <- paste(wrkdr,"/",HIR2[z],"35Grid/",job,"/",sep="")
                        dir.create(outDir,showWarnings = FALSE,recursive=TRUE)
                
                          ###### EXTRACT Sensor Zenit Angle
                        #        MRT <- paste(wrkdr,'/MRTkicker',rNum,'.prm',sep='')
                        #        filename = file(MRT, open='wt')
                          
                        #        write(paste('INPUT_FILENAME = ',MOD35File,sep=''), filename)
                        #        write(paste('GEOLOCATION_FILENAME = ',MOD03File,sep=''),filename)
                        #        write(paste('INPUT_SDS_NAME = Sensor_Zenith',sep=''),filename)
                        #        write(paste('OUTPUT_SPATIAL_SUBSET_TYPE = LAT_LONG',sep=''),filename)
                        #        write(paste('OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) = ',min(AOIpol[,'X']),' ',max(AOIpol[,'Y']),sep=''),filename)
                        #        write(paste('OUTPUT_SPACE_LOWER_RIGHT_CORNER (LONG LAT) = ',max(AOIpol[,'X']),' ',min(AOIpol[,'Y']),sep=''),filename)
                        #        write(paste('OUTPUT_FILENAME = ',outDir,HIR2[z],'35G_',datum,'_',time,'.tif',sep=''),filename)
                        #        write(paste('OUTPUT_FILE_FORMAT = GEOTIFF_FMT',sep=''),filename)
                        #        write(paste('KERNEL_TYPE (CC/BI/NN) = ',RT,sep=''),filename)
                        #        write(paste('OUTPUT_PROJECTION_NUMBER = ',Proj,sep=''),filename)
                        #        write(paste('OUTPUT_PROJECTION_PARAMETER = 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0',sep=''),filename)
                        #        write(paste('OUTPUT_PROJECTION_SPHERE = 8',sep=''),filename)
                        #        write(paste('OUTPUT_PROJECTION_ZONE = ',zone,sep=''),filename)
                        #        close(filename)
                        #
                        #        system(paste('swath2grid -pf=',MRT,sep='')) # extract MxD35
                        #
                        #        zV <- raster(paste(outDir,HIR2[z],'35G_',datum,'_',time,'_SensorZenith.tif',sep=''))
                        #        minmax <- range(zV[])
                
                        #        unlink(paste(outDir,HIR2[z],'35G_',datum,'_',time,'_SensorZenith.tif',sep=''))
                
                        #    if (abs(minmax[1]) < 30000) { # go on only if ZviewAnge is smaller than 30000, else next image!
                    
                        MRT <- paste(wrkdr,'MRTkicker',rNum,'.prm',sep='')
                        filename = file(MRT, open='wt')
        
                        write(paste('INPUT_FILENAME = ',MOD35File,sep=''), filename)
                        write(paste('GEOLOCATION_FILENAME = ',MOD03File,sep=''),filename)
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
                        write(paste('OUTPUT_PROJECTION_ZONE = ',zone,sep=''),filename)
                        close(filename)
                
                        system(paste('swath2grid -pf=',MRT,sep='')) # extract MxD35
                        unlink(MRT)
                        unlink(Files)
                        
                        tempCLM <- paste(outDir,HIR2[z],'35G_',datum,'_',time,'_Cloud_Mask_b0.tif',sep='')
                        CLM <- paste(outDir,HIR2[z],'35G_',datum,'_',time,'_ExtrCloudM.tif',sep='')

                        b0   <- raster(tempCLM)
                        msk <- b0==0
                        NAvalue(b0) <- -1
                        dat <- calc(b0,function(x) bitAnd(bitShiftR(x,1),3), forcefun=TRUE)
                        dat[msk] <- -1   
                        writeRaster(dat,filename=CLM, overwrite=TRUE)                     
                        unlink(tempCLM) # not more needed
                        
                        nC     <- ncell(b0)
                        free   <- sum(dat[] == 3)
                        pfree  <- sum(dat[] == 2)
                        pcloud <- sum(dat[] == 1)
                        cloud  <- sum(dat[] == 0)
                        NAs    <- sum(sign(dat[])==-1)
                        s=s+1
                        clResult[[s]]<- c(basename(MOD03File),free,pfree,pcloud,cloud,NAs,nC)#,minmax) 
                
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
                
                        x <- as.data.frame(clResult,ncol=length(clResult),byrow=TRUE)
                        #colnames(x)<- c("MOD03Filename","Free","probFree","probCloudy","Cloudy","NAs","nCell","MaxViewAngle","MinViewAngle")
                        write.csv2(x,paste("summaries_",HIR2[z],"_",job,".csv",sep=""))
                
                        #}   # if min zV>3000 close
                    } # in day process close [l]
                    rm(list35)
                    rm(list03)
                } # if intersect == 0
            } # if no files in filename[i] (geoMeta.txt-file)
        } # in period (startdate to enddate) process close [i]
        colnames(x)<- c("MOD03Filename","Free","probFree","probCloudy","Cloudy","NAs","nCell")#,"MaxViewAngle","MinViewAngle")
        write.csv2(x,paste("summaries_",HIR2[z],"_",job,".csv",sep=""))
    } # y , period sy:ey 
} # platform MOD/MYD switch close [z]      

} # end Function "run()"

