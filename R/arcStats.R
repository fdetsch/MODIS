# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : July 2012
# Licence GPL v3


arcStats <- function(product, collection=NULL, extent="global", begin="2000.01.01", end=format(Sys.time(), "%Y.%m.%d"), asMap=TRUE, outName=NULL,...)
{  
# product="MYD17A2"; collection="005"; extent=list(xmin=-20,xmax=40,ymin=10, ymax=20); begin="2000.08.01"; end="2000.08.21"; asMap="both"; outName=NULL;u=1;z=1
# product="MOD13Q1"; collection="005"; extent='global'; begin="2000.08.01"; end="2004.08.21"; asMap="both"; outName=NULL;u=1;z=1
 
    if (!require(rgdal))
    {
        stop("Please install 'rgdal': install.packages('rgdal')")
    }

    date4name <- format(Sys.time(), "%Y%m%d%H%M%S") 
          
    if(is.null(outName))
    {
        if (inherits(extent,"character"))
        {
            outName <- paste(paste0(extent,collapse=""),date4name,sep="_")
        } else 
        {
            outName <- date4name
        }
    }
    
    opts <- combineOptions(...)
    opts$localArcPath <- setPath(opts$localArcPath) 
    opts$outDirPath   <- setPath(opts$outDirPath) 
    
    if (opts$outProj == "asIn")
    {
        opts$outProj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"        
    } else 
    {
        opts$outProj <- checkOutProj(opts$outProj,tool="GDAL")
    }
    
    # product/dates/extent
    product     <- getProduct(x=product, quiet=TRUE)
    product$CCC <- getCollection(product=product, collection=collection, quiet=TRUE)
    tLimits     <- transDate(begin=begin, end=end)

    if (extent[1]=="global")
    {  
        ext <- getTile(extent=extent(raster()))
    } else 
    {
        ext <- getTile(extent=extent)
    }

    for (z in seq_along(product$PRODUCT))
    {
      prod <- product$PRODUCT[z]
      coll <- product$CCC[[which(names(product$CCC)==product$PRODUCT[z])]]
      todo <- paste0(prod,".", coll)
      
      ftpdirs  <- as.Date(getStruc(product=prod,collection=coll, begin=tLimits$begin, end=tLimits$end, wait=0)$dates)
      expected <- ftpdirs[tLimits$begin <= ftpdirs & tLimits$end >= ftpdirs]
      expected <- expected[!is.na(expected)]

      path <- genString(x=prod,collection=coll,remote=FALSE,...)$localPath
      path <- strsplit(path,"/")[[1]]
      path <- paste0(path[-length(path)],collapse="/")

      allLocal <- list.files(path=path,pattern=".hdf$",recursive=TRUE,full.names=TRUE)

      if(length(allLocal)==0)
      {
          warnings(paste0("No ", todo, " files found!"))
          locDates <- NULL
      } else 
      {
          # remove dates not in the query
          locDates <- sapply(allLocal,function(x)
          {
              date <- strsplit(normalizePath(dirname(x),winslash="/"),"/")[[1]]
              date <- date[length(date)]
              return(date)
          })
                      
          locDates <- as.Date(locDates,"%Y.%m.%d")
          locDates <- allLocal[locDates%in%expected]
      }
      
      # remove not requested tiles
      if(length(locDates)==0)
      {
          tileinfo <- ext$tile
      } else if (extent[1]=="global") 
      {
          tileinfo <- unique(sapply(basename(locDates),function(x){x <- getProduct(x);getPart(x,"TILE")}))
      } else
      {
          tileinfo <- ext$tile
      }
                           
      available <- needed <- percent <- rep(0,length(tileNames))
      MBperHDF  <- rep(NA,length(available))
      table     <- data.frame(tileNames,available,needed,percent,MBperHDF)
      table[,"needed"] <- length(expected)
      colnames(table)  <- c("tile", "available", "needed", "percent", "MBperHDF")
                      
      # calculation section
      for (i in seq_along(tileinfo))
      {
          n <- grep(locDates,pattern=tileinfo[i],value=TRUE)
          
          if (length(n)!=0)
          {
              meanSize <- mean(file.size(n,units="Mb")) 
              n <- sapply(n,function(x)
              {
                  date <- strsplit(normalizePath(dirname(x),winslash="/"),"/")[[1]]
                  date <- date[length(date)]
                  return(date)
              })
                  
              n <- as.Date(n,"%Y.%m.%d")
              n <- sum(n %in% expected)
          } else 
          {
              n <- 0
              meanSize <- NA 
          }
              
          ind <- which(tileNames==tileinfo[i])
          table[ind,"available"] <- n
          table[ind,"percent"]   <- round(100*(n/length(expected)),2)
          table[ind,"MBperHDF"]  <- round(meanSize)
      }

      # mapping 
      if (isTRUE(asMap)|tolower(asMap)=="both")
      {
#                if (!(require(maptools)))
#                {
#                    stop("Please install maptools package: install.packages('maptools')")
#                }
          
          if (!(require(mapdata)))
          {
              stop("Please install mapdata package: install.packages('mapdata')")
          }

          srx <- sr
          srx@data <- data.frame(percent=(round(table$percent)))
           
          # require(scales)
          # colors <- c("#00000000",colorRampPalette(c("red","blue","green"))(100)) # hollow + palette
          colors <- c("#00000000", "#FF0000", "#F90005", "#F4000A", "#EF000F", "#EA0014", "#E50019", "#E0001E", "#DA0024", "#D50029", "#D0002E", "#CB0033", "#C60038", "#C1003D", "#BC0042", "#B60048", "#B1004D", "#AC0052", "#A70057", "#A2005C", "#9D0061", "#970067", "#92006C", "#8D0071", "#880076", "#83007B", "#7E0080", "#790085", "#73008B", "#6E0090", "#690095", "#64009A", "#5F009F", "#5A00A4", "#5400AA", "#4F00AF", "#4A00B4", "#4500B9", "#4000BE", "#3B00C3", "#3600C8", "#3000CE", "#2B00D3", "#2600D8", "#2100DD", "#1C00E2", "#1700E7", "#1200EC", "#0C00F2", "#0700F7", "#0200FC", "#0002FC", "#0007F7", "#000CF2", "#0012EC", "#0017E7", "#001CE2", "#0021DD", "#0026D8", "#002BD3", "#0030CE", "#0036C8", "#003BC3", "#0040BE", "#0045B9", "#004AB4", "#004FAF", "#0055A9", "#005AA4", "#005F9F", "#00649A", "#006995", "#006E90", "#00738B", "#007985", "#007E80", "#00837B", "#008876", "#008D71", "#00926C", "#009767", "#009D61", "#00A25C", "#00A757", "#00AC52", "#00B14D", "#00B648", "#00BC42", "#00C13D", "#00C638", "#00CB33", "#00D02E", "#00D529", "#00DA24", "#00E01E", "#00E519", "#00EA14", "#00EF0F", "#00F40A", "#00F905", "#00FF00")

          
          xlim <- c(ext$extent@xmin,ext$extent@xmax)
          ylim <- c(ext$extent@ymin,ext$extent@ymax)
          
          xax <- data.frame(x=seq(-180,180,by=10),y=rep(0,37),tileID=c(0:35,""))
          yax <- data.frame(x=rep(0,19),y=seq(90,-90,by=-10),tileID=c(0:17,""))
          
          png(paste(opts$outDirPath,todo,".",outName,".png",sep=""), width = 800, height = 600)
                 
          if(extent[1]=="global")
          {
              globe <- map("world",plot=FALSE)
          } else
          {
              globe <- map("worldHires",plot=FALSE,xlim=xlim,ylim=ylim)
              xax   <- xax[xax$x>=min(xlim) & xax$x<=max(xlim),]
              yax   <- yax[yax$y>=min(ylim) & yax$y<=max(ylim),]
          }  
          coordinates(xax) <- ~x+y
          coordinates(yax) <- ~x+y 
          proj4string(xax) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
          proj4string(yax) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"                               
          
          globe$x[!is.na(globe$x) & globe$x > 180] <- 180
          
          # m2SL is maptools:::map2spatialLines, see getTile.R                  
          globe <- m2SL(globe)

          #invisible(set_ll_warn(TRUE)) # shouldn't be necessary becaus of the trimming
          #iwa <- options()$warn
          #options(warn=-1)
          proj4string(globe) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
          #options(warn=iwa)
             
          if(!isLonLat(opts$outProj))
          {
              tmp <- raster(ext$extent)
              projection(tmp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
              tmp <- projectExtent(tmp,opts$outProj)
               
              xlim <- c(xmin(tmp),xmax(tmp))
              ylim <- c(ymin(tmp),ymax(tmp))
              
              xax <- spTransform(xax,CRS(opts$outProj))
              yax <- spTransform(yax,CRS(opts$outProj))

              globe <- spTransform(globe,CRS(opts$outProj))
              srx   <- spTransform(srx,CRS(opts$outProj))
          }
          
          xax <- as.data.frame(xax)[,c("x","tileID")]
          yax <- as.data.frame(yax)[,c("y","tileID")]
          
          globe <- list("sp.lines", as(globe, "SpatialLines"), lwd=0.8)                    
          
           
          if(opts$outProj=="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
          {
              print(
                  spplot(srx, zcol="percent", col.regions=colors, colorkey=TRUE,at=0:101,xlab="H-Tiles",ylab="V-Tiles", xlim=xlim, ylim=ylim, sp.layout=globe,
                  scales=list(alternating=3,x=list(at=xax$x ,labels=xax$tileID,rot=90),y=list(at=yax$y,labels=yax$tileID)),
                  main = paste("Percentage of '",todo,"' available on the local archive\nbetween ", tLimits$begin," and ", tLimits$end,sep=""), checkEmptyRC=FALSE)
              )

          } else
          {
              print(
                  spplot(srx, zcol="percent", col.regions=colors, colorkey=TRUE,at=0:101, xlim=xlim, ylim=ylim, sp.layout=globe, scales=list(draw = TRUE),
                  main = paste("Percentage of '",todo,"' available on the local archive\nbetween ", tLimits$begin," and ", tLimits$end,sep=""), checkEmptyRC=FALSE)
                  )
          }
          dev.off()
      }
      if (!isTRUE(asMap)|asMap=="both")
      {
          if (extent[1]=="global")
          {
              out <- table
          } else 
          {
              out <- table[table$tile %in% ext$tile,]
          }
          write.csv(x=out,file=paste(opts$outDirPath,todo,".",outName,".csv",sep=""),row.names = FALSE)                
      }
  }
return(invisible(NULL))
}
