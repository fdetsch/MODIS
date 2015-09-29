# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2012
# Licence GPL v3

# maybe to add: derivate=1

# vx=val[index,u];wx=wtu[index,u];tx=inTu[index,u]
# tmtr <- cbind(vx,wx,tx)

unifyDoubleWM <- function(vx,wx,tx)
{
  tx <- as.numeric(tx)
  vx <- as.numeric(vx)
  wx <- as.numeric(wx)

  double <- tx[duplicated(tx)]
  
  if(length(double)>0)
  {
    double <- unique(double)

    for(i in seq_along(double))
    {
      inx        <- which(tx==double[i])
      vx[inx[1]] <- weighted.mean(vx[inx],w=wx[inx])
      wx[inx[1]] <- max(wx[inx])
      vx         <- vx[-inx[-1]]
      wx         <- wx[-inx[-1]]
      tx         <- tx[-inx[-1]]
    }
  }
  list(vx=vx,wx=wx,tx=tx)
}

unifyDoubleMX <- function(vx,wx,tx)
{
  tx <- as.numeric(tx)
  vx <- as.numeric(vx)
  wx <- as.numeric(wx)

  double <- tx[duplicated(tx)]
  
  if(length(double)>0)
  {
    double <- unique(double)

    for(i in seq_along(double))
    {
      inx <- which(tx==double[i])
      mx  <- which.max(wx[inx])
      vx  <- vx[-inx[-mx]]
      wx  <- wx[-inx[-mx]]
      tx  <- tx[-inx[-mx]]
    }
  }
  list(vx=vx,wx=wx,tx=tx)
}


doCollapse <- function(tx,pillow)
{
  ord <- order(tx)
  txS <- tx[ord]
  
  t0 <- 365 - pillow
  
  tS <- ord[txS >= t0]
  tE <- ord[txS <= pillow]
  
  s0 <- txS[txS >= t0] - t0 
  s1 <- txS + pillow
  s2 <- txS[txS <= pillow] + 365 + pillow 
  
  list(order=c(tS,ord,tE),sequence=c(s0,s1,s2)+1)
}
    
whittaker.raster <- function(vi, w=NULL, t=NULL, timeInfo = orgTime(vi), lambda = 5000, nIter= 3, outputAs="single", collapse=FALSE, prefixSuffix=c("MCD","ndvi"), outDirPath=".", outlierThreshold=NULL, mergeDoyFun="max", ...)
{
  if(!require(ptw))
  {
    stop("In order to use the the whittaker filter please install 'ptw': install.package('ptw')") 
  }

  # debug 
  # w=wt; t=inT; groupYears=TRUE; lambda = 5000; nIter= 3; outDirPath = "./"; collapse=FALSE; opt <- MODIS:::combineOptions();removeOutlier=FALSE;mergeDoyFun="max";opts$outDirPath <- MODIS:::setPath(outDirPath)
  # w=NULL; t=NULL; groupYears=TRUE; lambda = 5000; nIter= 5; outDirPath = "./SUB/";collapse=TRUE; opts <- MODIS:::combineOptions();removeOutlier=FALSE;mergeDoyFun="max"

  # opt <- list(bitShift=2,bitMask=15,threshold=6)
  # opt <- list(bitShift=2,bitMask=15)
  # opt <- list()
      
  opts <- combineOptions(...)
  
  outDirPath     <- setPath(outDirPath)
  bitShift       <- opts$bitShift
  bitMask        <- opts$bitMask
  threshold      <- opts$threshold

  dataFormat     <- opts$dataFormat
  rasterOut      <- toupper(writeFormats())
    
  if(toupper(dataFormat) %in% rasterOut[,"name"])
  {
    dataFormat <- getExtension(dataFormat)
  } else
  {
    stop("Argument dataFormat='",dataFormat,"' is unknown/not supported. Please run 'writeFormats()' (column 'name') so list available dataFormat's")
  }

  if (is.null(opts$minDat))
  {
    minDat <- 3 # 3 is very small!
  } else 
  {
    minDat <- opts$minDat
  }
        
  if(collapse)
  {
    if(timeInfo$call$nDays=="asIn")
    {
      stop("'orgTime' argument nDays='asIn' is not allowed when using collapse=TRUE")
    }
    fitt <- seq(as.numeric(format(min(timeInfo$outputLayerDates),"%j")),as.numeric(format(max(timeInfo$outputLayerDates),"%j")),by=timeInfo$call$nDays) + timeInfo$call$pillow    
  } else
  {
    fitt <- timeInfo$outSeq
  }

  inlam  <- lambda
  if (is.character(lambda))
  {
    cat("Using fixed 'lambda':",lambda,"\n")
    nameL <- "fL"
  } else 
  {
    if (collapse)
    {
      lambda <- lambda*((365 + 2*timeInfo$call$pillow)/365)
      cat("Yearly 'lambda' is:",inlam,"\nNow changed with lambda*((365+2*pillow)/365) to:",lambda,"\n")
    } else
    {
      lambda <- lambda*((max(timeInfo$inSeq) - min(timeInfo$inSeq) -1)/365)
      cat("Yearly 'lambda' is:",inlam,"\nNow changed with lambda*('length of input data period in days'/365) to:",lambda,"\n")
    }
    nameL <- "yL"
  }
  inlam  <- round(inlam) #from here on used only in outputfilename
  lambda <- as.numeric(lambda)

  if(!inherits(vi,"Raster")) 
  {
    vi <- stack(vi,quick=TRUE)
  }

  if(!inherits(w,"Raster") & !is.null(w)) 
  {
    w <- stack(w,quick=TRUE)
  }

  if(!inherits(t,"Raster") & !is.null(t)) 
  {
    t <- stack(t,quick=TRUE)
  }
  
  if(is.null(opts$datatype))
  {
    opts$datatype <- dataType(vi[[1]])
  }

  if(opts$datatype == "FLT")
  {
    doround <- FALSE
  } else
  {
    doround <- TRUE
  }
  
  if (is.null(opts$overwrite))
  {
    opts$overwrite <- FALSE
  }
 
  outputAs <- tolower(outputAs)
  if (collapse) # use only DOY obmitt year (all years into one)
  {
    d <- sprintf("%03d",seq(as.numeric(format(min(timeInfo$outputLayerDates),"%j")),as.numeric(format(max(timeInfo$outputLayerDates),"%j")),by=timeInfo$call$nDays))
    if(outputAs=="single")
    {
      oname <- paste0(outDirPath,prefixSuffix[1],".",d,".",prefixSuffix[2],dataFormat)
      b     <- vector(mode="list",length=length(oname)) 
      for(a in seq_along(oname))
      {      
        b[[a]] <- raster(vi)
        b[[a]] <- writeStart(b[[a]], filename=oname[a], datatype=opts$datatype, overwrite=opts$overwrite)
      }
    } else
    {
        b      <- list()
        b[[1]] <- writeStart(brick(raster(vi),nl=length(d)), filename=oname, datatype=opts$datatype, overwrite=opts$overwrite)
        names(b[[1]]) <- paste0("doy",d)
    }
  } else if (outputAs=="yearly")
  {
    b <- vector(mode="list",length=length(unique(format(timeInfo$outputLayerDates,"%Y")))) 
    for (a in seq_along(unique(format(timeInfo$outputLayerDates,"%Y"))))
    {
      y <- unique(format(timeInfo$outputLayerDates,"%Y"))[a]
      oname  <- paste0(outDirPath,prefixSuffix[1],".year",y,".",nameL,inlam,".",prefixSuffix[2],dataFormat)
      names  <- timeInfo$outputLayerDates[format(timeInfo$outputLayerDates,"%Y")==y]
      b[[a]] <- brick(raster(vi),nl=as.integer(sum(format(timeInfo$outputLayerDates,"%Y")==y)), values=FALSE)
      b[[a]] <- writeStart(b[[a]], filename=oname, datatype=opts$datatype, overwrite=opts$overwrite)
      names(b[[a]]) <- names
    }
  } else if(outputAs=="one") 
  {
    y      <- unique(format(timeInfo$outputLayerDates,"%Y"))
    oname  <- paste0(outDirPath,prefixSuffix[1],"_from",paste0(y,collapse="to"),".",nameL,inlam,".",prefixSuffix[2],dataFormat)
    
    b      <- list()
    b[[1]] <- brick(raster(vi),nl=length(fitt), values=FALSE)  
    b[[1]] <- writeStart(b[[1]], filename=oname, datatype=opts$datatype, overwrite=opts$overwrite)
    names(b[[a]]) <- timeInfo$outputLayerDates
    
  } else if (outputAs=="single")
  {
    d     <- sort(format(timeInfo$outputLayerDates,"%Y%j"))
    oname <- paste0(outDirPath,prefixSuffix[1],".",d,".",nameL,inlam,".",prefixSuffix[2],dataFormat)
    b     <- vector(mode="list",length=length(oname)) 
    for(a in seq_along(oname))
    {      
      b[[a]] <- raster(vi)
      b[[a]] <- writeStart(b[[a]], filename=oname[a], datatype=opts$datatype, overwrite=opts$overwrite)
    }
  } 
  
  tr <- blockSize(vi)
  cluster <- isTRUE(getOption("rasterCluster"))
  
  if (cluster)
  {
    # beginCluster()
    cl <- getCluster()
    on.exit(endCluster())
    nodes <- getOption("rasterClusterCores")

    # MODIS fails to load if not done like that ...        
    clF <- function(i){require(MODIS)}
    for (i in 1:nodes) 
    {
      parallel:::sendCall(cl[[i]], clF, i, tag=i)
      parallel:::recvOneData(cl)
    }
    
    # better to be save than sorry:
    parallel:::clusterEvalQ(cl,require(bitops))
    parallel:::clusterEvalQ(cl,require(rgdal))
    parallel:::clusterEvalQ(cl,require(ptw))
    tr <- blockSizeCluster(vi)
  }    

  cat("Data is in, start processing!\n")

  if(tolower(mergeDoyFun)=="max")
  {
    mergeFun <- unifyDoubleMX
  } else if (tolower(mergeDoyFun)=="weighted.mean" | tolower(mergeDoyFun)== "mean")
  {
    mergeFun <- unifyDoubleWM
  }
  
  clFun <- function(l)
  {
    val    <- getValues(vi, row=tr$row[l], nrows=tr$nrows[l])
    val    <- t(val)
    mtrdim <- dim(val)

    set0   <- matrix(FALSE,nrow=mtrdim[1], ncol=mtrdim[2])
    set0[is.na(val)] <- TRUE
        
    if (!is.null(w))
    {
      wtu <- getValues(w, row=tr$row[l], nrows=tr$nrows[l])
      
      # is it not a weight info [0-1]?
      if(max(wtu,na.rm=TRUE) > 1)
      {
        if(is.null(bitShift) | is.null(bitMask))
        {
          # try to detect VI usefulness layer
          bits     <- detectBitInfo(vi,"VI usefulness",warn=FALSE)
          bitShift <- bits$bitShift
          bitMask  <- bits$bitMask
        }
        if(is.null(bitShift) | is.null(bitMask))
        {
          stop("Could not extract 'bits' for weighting from this product. Use '?makeWeights' function to generate weights manually!")
        }
        wtu  <- makeWeights(wtu, bitShift = bitShift, bitMask = bitMask, threshold = threshold, decodeOnly = FALSE)
      }
      wtu <- t(wtu)
      set0[wtu==0] <- TRUE
    } else
    {
      # if no weighting info is available then weight = 1
      wtu <- matrix(1,nrow=mtrdim[1],ncol=mtrdim[2])
    }
    
    if (inherits(t,"Raster"))
    {
      inTu <- getValues(t, row=tr$row[l], nrows=tr$nrows[l])
      inTu <- t(inTu)
            
      set0[is.na(inTu)] <- TRUE
      set0[ inTu <= 0 ] <- TRUE

      t0 <- min(timeInfo$inDoys[1]) - 1
            
      if(!collapse)
      {
        inTu <- t(repDoy(t(inTu),layerDate=timeInfo,bias=-t0))
      }
      inTu[set0] <- 0
    } else 
    {
      if (collapse)
      {
        inTu <- matrix(timeInfo$inDoys,nrow=length(timeInfo$inDoys),ncol=mtrdim[2])
      } else
      {       
        inTu <- matrix(timeInfo$inSeq,nrow=length(timeInfo$inSeq),ncol=mtrdim[2])
      }
    }

    # the entire info to use or not a pix is in "wtu"
    wtu[set0] <- 0
    val[set0] <- 0    

    out <- matrix(NA, nrow=length(fitt), ncol=mtrdim[2])
    
    if(!is.null(outlierThreshold))
    {
      kickOutlier <- function(vals,weights,lambda,threshold)
      {
        fTS <- whit2(vals,w=weights,lambda=lambda)
        weights[weights==1][abs(vals[weights==1]-fTS[weights==1]) > threshold] <- 0            
        return(weights)    
      }
    } else
    {
      # if is.null(outlierThreshold) generate a fake function to avoid a per pixel "if"
      kickOutlier <- function(vals,weights,lambda,threshold)
      {
        return(weights)
      }       
    }

    if (collapse)
    {
      vec0 <- rep(0,365 + (2*timeInfo$call$pillow) + 30) # add a save length of data (because layer doy + effectice composite doy)
    } else
    {
      vec0 <- rep(0,max(timeInfo$inSeq,timeInfo$outSeq) - min(timeInfo$inSeq,timeInfo$outSeq) - 1  + 30)
    }
    # minimum "minDat" input values for filtering 
    Cvec <- (colSums(wtu > 0) >= minDat)
    Cvec <- (1:mtrdim[2])[Cvec]
    ind  <- inTu > 0    
    
    win <- options("warn")
    options(warn=-1)
   
    for (u in Cvec)
    {   
      index  <- ind[,u]
      use    <- mergeFun(vx=val[index,u],wx=wtu[index,u],tx=inTu[index,u])    
      
      valVec <- wtVec <- vec0
      
      if(!collapse)
      {
        valVec[use$tx] <- use$vx
        wtVec[use$tx]  <- use$wx
      } else
      {
        newOrder <- doCollapse(tx=use$tx,pillow=timeInfo$call$pillow)
        valVec[newOrder$sequence] <- use$vx[newOrder$order]
        wtVec[newOrder$sequence]  <- use$wx[newOrder$order]  
      }
      wtVec <- kickOutlier(vals=valVec,weights=wtVec,lambda=lambda,threshold=outlierThreshold)
      #plot(valVec,ylim=c(-1000,9000))
      for(i in 1:nIter)
      {
        fTS <- whit2(valVec,w=wtVec,lambda=lambda)
        valVec[valVec < fTS] <- fTS[valVec < fTS]
      }
      out[,u] <- fTS[fitt]
      #lines(fTS,col=2)
    }
    options(warn=win$warn)
    out[,colSums(abs(out))==0] <- NA
  return(t(out))
  }

  if (!cluster)
  {    
    for (i in seq_along(tr$row))
    {    
      res <- clFun(i)
      
      if(doround)
      {
        res <- round(res)
      }
      b <- writeValuesMODIS(b,res,tr$row[i],timeInfo,collapse,outputAs)
    }       
  } else
  {
    for (i in 1:nodes) 
    {
        parallel:::sendCall(cl[[i]], fun = clFun, args = i, tag=i)
    }

    for (i in 1:tr$n)
    {
      d <- parallel:::recvOneData(cl)

      if (!d$value$success)
      {
          stop("cluster error")
      }

      ni <- nodes + i
      if (ni <= tr$n)
      {
        parallel:::sendCall(cl[[d$node]], fun = clFun, args = ni, tag=ni)
      }
      
      if(doround)
      {
        d$value$value <- round(d$value$value)
      }

      #####
      b <- writeValuesMODIS(b,res,tr$row[d$value$tag],timeInfo,collapse,outputAs)
    }
  }
  writeStopMODIS(b,timeInfo,outputAs,collapse)
  return(b)
}


#####################################################

writeValuesMODIS <- function(b,val,row,timeInfo,collapse,outputAs)
{
  if(collapse)
  {
    d <- seq_along(sprintf("%03d",seq(as.numeric(format(min(timeInfo$outputLayerDates),"%j")),as.numeric(format(max(timeInfo$outputLayerDates),"%j")),by=timeInfo$call$nDays)))
  } else
  {
    d <- seq_along(b)
  }
  if(outputAs=="single")
  {
    for (a in seq_along(d))
    {
      b[[a]] <- writeValues(b[[a]], val[,a], row)
    }   
  } else if(outputAs=="one")
  {
    b[[1]]  <- writeValues(b[[1]], val, row)  
  } else
  {
    for (a in seq_along(d))
    {
      y      <- unique(format(timeInfo$outputLayerDates,"%Y"))[a]
      b[[a]] <- writeValues(b[[a]], val[,format(timeInfo$outputLayerDates,"%Y")==y], row)
    }   
  }
return(b)
}

writeStopMODIS <- function(b,timeInfo,outputAs,collapse)
{
  for (a in seq_along(b))
  {    
    b[[a]] <- writeStop(b[[a]])
    
    nam <- filename(b[[a]])
    extension(nam) <- ""

    if (collapse & outputAs!="single")
    {
      write.table(x=unique(format(timeInfo$outputLayerDates,"%j")),
      file=nam, col.names=FALSE, row.names=FALSE)
    } else if(outputAs=="one")
    {
      write.table(x=timeInfo$outputLayerDates, file=nam,
      col.names=FALSE, row.names=FALSE)
    } else if (outputAs=="yearly")
    {
      y <- unique(format(timeInfo$outputLayerDates,"%Y"))
      for (ax in seq_along(y))
      {
        ind <- format(timeInfo$outputLayerDates,"%Y")==y[ax]
        write.table(x=timeInfo$outputLayerDates[ind], file=nam,
        col.names=FALSE, row.names=FALSE)
      } 
    }     
  }
}

