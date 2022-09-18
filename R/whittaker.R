#' Filter Vegetation Index with Modified Whittaker Approach
#' 
#' @description 
#' Use a modified Whittaker filter function (see References) from package 
#' **ptw** to filter a vegetation index (VI) time series of satellite data.
#' 
#' @param vi `Raster*` or `character` file names, sorted VI. Use [preStack()] 
#'   functionality to ensure the right input.
#' @param w `Raster*` or `character` file names. In case of MODIS composite, the
#'   sorted 'VI_Quality' layers.
#' @param t `Raster*` or `character` file names. In case of MODIS composite, the
#'   sorted 'composite_day_of_the_year' layers. If missing, the date is 
#'   determined using 'timeInfo'.
#' @param timeInfo Output from [orgTime()].
#' @param lambda `character` or `integer`. Yearly lambda value passed to 
#'   [ptw::whit2()]. If set as `character` (i.e., `lambda = "600"`), it is not 
#'   adapted to the time series length, but used as a fixed value (see Details).
#'   High values = stiff/rigid spline.
#' @param nIter `integer`. Number of iterations for the upper envelope fitting.
#' @param outputAs `character`, organization of output files. `"single"` 
#'   (default) means each date one `RasterLayer`; `"yearly"` a `RasterBrick` for
#'   each year, and `"one"` one `RasterBrick` for the entire time series.
#' @param collapse `logical`. Collapse input data of multiple years into one 
#'   single year before filtering.
#' @param prefixSuffix `character`, file naming. Names are dot-separated: 
#'   `paste0(prefixSuffix[1], "YYYDDD", lambda, prefixSuffix[2], ".defaultFileExtension")`.
#' @param outDirPath `character`, output path. Defaults to the current working 
#'   directory.
#' @param outlierThreshold `numeric` in the same unit as 'vi', used for outlier 
#'   removal (see Details).
#' @param mergeDoyFun Especially when using `collapse = TRUE`, multiple 
#'   measurements for one day can be present. Here you can choose how those 
#'   values are merged to one single value: `"max"` uses the highest value, 
#'   `"mean"` or `"weighted.mean"` use [mean()] or [stats::weighted.mean()].
#' @param ... Arguments passed to [raster::writeRaster()] (except for 
#'   'filename').
#' 
#' @return 
#' A Whittaker-smoothed `RasterStack`.
#' 
#' @details 
#' The argument 'lambda' is passed to `MODIS:::miwhitatzb1`. You can set it as 
#' yearly 'lambda', which means that it doesn't matter how long the input time 
#' series is because 'lambda' is adapted to it with: 
#' `lambda * ('length of input time series in days' / 365)`. The input length 
#' can differ from the output because of the 'pillow' argument in [orgTime()]. 
#' But it can also be set as `character` (i.e., `lambda = "1000"`). In this 
#' case, the adaption to the time series length is not performed.
#' 
#' @references 
#' Modified Whittaker smoother, according to Atzberger & Eilers 2011 
#' International Journal of Digital Earth 4(5):365-386, 
#' \doi{10.1080/17538947.2010.505664}.
#' Implementation in R: Agustin Lobo 2012
#' 
#' @note 
#' Currently tested on MODIS and Landsat data. Using M*D13, it is also possible 
#' to use the 'composite_day_of_the_year' and the 'VI_Quality' layers.
#' 
#' @seealso 
#' [smooth.spline.raster()], [raster::raster()].
#' 
#' @author 
#' Matteo Mattiuzzi and Agustin Lobo
#' 
#' @examples 
#' \dontrun{
#' # The following function will download bit more than 1 year of MOD13A1 (~180mB) and therefore
#' # take while to execute! Data will be downloaded to options("MODIS_localArcPath") and processed 
#' # to 'paste0(options("MODIS_outDirPath"),"fullCapa")'
#' # You need to extract: 'vegetation index', 'VI_Quality layer', and 'composite day of the year',
#' # this is expressed by the argument 'SDSstring'
#' runGdal(product="MOD13A2",begin="2004340",extent="ireland",end="2006020", job="fullCapa",
#' SDSstring="101000000010")
#' path <- paste0(options("MODIS_outDirPath"),"fullCapa")
#' 
#' # the only obligatory dataset is the vegetatino index 
#' # get the 'vi' data in the source directory: 
#' vi <- preStack(path=path, pattern="*_NDVI.tif$")
#' 
#' # "orgTime" detects timing information of the input data and generates based on the arguments
#' # the output date information. 
#' # For spline functions (in general) the beginning and the end of the time series
#' # is always problematic. So there is the argument "pillow" (default 75 days) that adds
#' # (if available) some more layers on the two endings.
#' timeInfo <- orgTime(vi,nDays=16,begin="2005001",end="2005365",pillow=40)
#' 
#' # now re-run "preStack" with two differences, 'files' (output of the first 'preStack' call)
#' # and the 'timeInfo'
#' # Here only the data needed for the filtering is extracted:
#' vi <- preStack(files=vi,timeInfo=timeInfo)
#' 
#' whittaker.raster(vi,timeInfo=timeInfo,lambda=5000)
#' 
#' # if the files are M*D13 you can use also Quality layers and the composite day of the year:
#' wt <- preStack(path=path, pattern="*_VI_Quality.tif$", timeInfo=timeInfo)
#' # can also be already stacked:
#' inT <- preStack(path=path, pattern="*_composite_day_of_the_year.tif$", timeInfo=timeInfo)
#' 
#' whittaker.raster(vi=vi, wt=wt, inT=inT, timeInfo=timeInfo, lambda=5000, overwrite=TRUE)
#' }
#' 
#' @name whittaker.raster
#' @export whittaker.raster
whittaker.raster <- function(vi, w=NULL, t=NULL, timeInfo = orgTime(vi), lambda = 5000, nIter= 3, outputAs="single", collapse=FALSE, prefixSuffix=c("MCD","ndvi"), outDirPath=".", outlierThreshold=NULL, mergeDoyFun="max", ...)
{

  opts <- combineOptions(...)

  outDirPath     <- setPath(outDirPath)
  bitShift       <- opts$bitShift
  bitMask        <- opts$bitMask
  threshold      <- opts$threshold
  
  dataFormat     <- opts$dataFormat
  rasterOut      <- toupper(raster::writeFormats())
  
  if(!toupper(dataFormat) %in% rasterOut[,"name"]) {
    stop("Unknown or unsupported data format: '", dataFormat, "'. Please run 
         raster::writeFormats() (column 'name') for supported file types.\n")
  }
  
  minDat <- ifelse(is.null(opts$minDat), 3, opts$minDat) # 3 is very small!
  
  if (collapse) {
    if(timeInfo$call$nDays == "asIn")
      stop("Argument nDays = 'asIn' (passed to orgTime()) is not allowed when using collapse = TRUE.\n")

      fitt <- seq(as.numeric(format(min(timeInfo$outputLayerDates),"%j")),as.numeric(format(max(timeInfo$outputLayerDates),"%j")),by=timeInfo$call$nDays) + timeInfo$call$pillow    
  } else {
    fitt <- timeInfo$outSeq
  }
  
  inlam  <- lambda
  
  ## fixed lambda
  if (is.character(lambda)) {
    cat("Using fixed 'lambda': ", lambda, ".\n", sep = "")
    nameL <- "fL"
    
  ## yearly lambda  
  } else {
    if (collapse) {
      lambda <- lambda * ((365 + 2 * timeInfo$call$pillow) / 365)
      cat("Yearly 'lambda' is:", inlam, "\nNow changed with lambda*((365+2*pillow)/365) to:",lambda,"\n")
    } else {
      lambda <- lambda * ((max(timeInfo$inSeq) - min(timeInfo$inSeq) - 1) / 365)
      cat("Yearly 'lambda' is: ", inlam, ".\n", 
          "Now changed to lambda * ('length of input period in days' / 365): ", 
          lambda, ".\n", sep = "")
    }
    
    nameL <- "yL"
  }
  
  if (is.character(inlam)) 
    inlam <- as.numeric(inlam)
  inlam  <- round(inlam) #from here on used only in outputfilename
  
  lambda <- as.numeric(lambda)
  
  if (!inherits(vi, "Raster")) 
    vi <- raster::stack(vi, quick = TRUE)

  if(!inherits(w, "Raster") & !is.null(w)) 
    w <- raster::stack(w, quick = TRUE)

  if(!inherits(t, "Raster") & !is.null(t)) 
    t <- raster::stack(t, quick = TRUE)

  if (is.null(opts$datatype)) 
    opts$datatype <- raster::dataType(vi[[1]])

  if (grepl("FLT", opts$datatype)) {
    doround <- FALSE
  } else {
    doround <- TRUE
  }
  
  if (is.null(opts$overwrite)) {
    opts$overwrite <- FALSE
  }
  
  outputAs <- tolower(outputAs)
  if (collapse) # use only DOY obmitt year (all years into one)
  {
    d <- sprintf("%03d",seq(as.numeric(format(min(timeInfo$outputLayerDates),"%j")),as.numeric(format(max(timeInfo$outputLayerDates),"%j")),by=timeInfo$call$nDays))
    if(outputAs=="single")
    {
      oname <- paste0(outDirPath,prefixSuffix[1],".",d,".",prefixSuffix[2])
      b     <- vector(mode="list",length=length(oname)) 
      for(a in seq_along(oname))
      {      
        b[[a]] <- raster(vi)
        b[[a]] <- writeStart(b[[a]], filename=oname[a], datatype=opts$datatype
                             , overwrite=opts$overwrite, format = dataFormat)
      }
    } else
    {
      b      <- list()
      b[[1]] <- writeStart(brick(raster(vi),nl=length(d)), filename=oname, datatype=opts$datatype
                           , overwrite=opts$overwrite, format = dataFormat)
      names(b[[1]]) <- paste0("doy",d)
    }
  } else if (outputAs=="yearly")
  {
    b <- vector(mode="list",length=length(unique(format(timeInfo$outputLayerDates,"%Y")))) 
    for (a in seq_along(unique(format(timeInfo$outputLayerDates,"%Y"))))
    {
      y <- unique(format(timeInfo$outputLayerDates,"%Y"))[a]
      oname  <- paste0(outDirPath,prefixSuffix[1],".year",y,".",nameL,inlam,".",prefixSuffix[2])
      names  <- timeInfo$outputLayerDates[format(timeInfo$outputLayerDates,"%Y")==y]
      b[[a]] <- brick(raster(vi),nl=as.integer(sum(format(timeInfo$outputLayerDates,"%Y")==y)), values=FALSE)
      b[[a]] <- writeStart(b[[a]], filename=oname, datatype=opts$datatype
                           , overwrite=opts$overwrite, format = dataFormat)
      names(b[[a]]) <- names
    }
  } else if(outputAs=="one") 
  {
    y      <- unique(format(timeInfo$outputLayerDates,"%Y"))
    oname  <- paste0(outDirPath,prefixSuffix[1],"_from",paste0(y,collapse="to"),".",nameL,inlam,".",prefixSuffix[2])
    
    b      <- list()
    b[[1]] <- brick(raster(vi),nl=length(fitt), values=FALSE)  
    b[[1]] <- writeStart(b[[1]], filename=oname, datatype=opts$datatype
                         , overwrite=opts$overwrite, format = dataFormat)
    names(b[[1]]) <- timeInfo$outputLayerDates
    
  } else if (outputAs=="single")
  {
    d     <- sort(format(timeInfo$outputLayerDates,"%Y%j"))
    oname <- paste0(outDirPath,prefixSuffix[1],".",d,".",nameL,inlam,".",prefixSuffix[2])
    b     <- vector(mode="list",length=length(oname)) 
    for(a in seq_along(oname))
    {      
      b[[a]] <- raster::raster(vi)
      b[[a]] <- writeStart(b[[a]], filename=oname[a], datatype=opts$datatype
                           , overwrite=opts$overwrite, format = dataFormat)
    }
  } 
  
  tr <- raster::blockSize(vi)
  
  cat("Data is in, start processing!\n")
  
  if (mergeDoyFun == "max") {
    mergeFun <- unifyDoubleMX
  } else if (mergeDoyFun == "weighted.mean" | mergeDoyFun == "mean") {
    mergeFun <- unifyDoubleWM
  }
  
  clFun <- function(l) {
    val <- raster::getValues(vi, row = tr$row[l], nrows = tr$nrows[l])
    val <- t(val)
    mtrdim <- dim(val)
    
    set0 <- matrix(FALSE,nrow = mtrdim[1], ncol = mtrdim[2])
    set0[is.na(val)] <- TRUE
    
    ## if 'VI_Quality' is supplied:
    if (!is.null(w)) {
      wtu <- raster::getValues(w, row = tr$row[l], nrows = tr$nrows[l])
      
      # is it not a weight info [0-1]?
      if (max(wtu, na.rm = TRUE) > 1) {
        if(is.null(bitShift) | is.null(bitMask)) {
          # try to detect VI usefulness layer
          bits     <- detectBitInfo(vi, "VI usefulness", warn = FALSE)
          bitShift <- bits$bitShift
          bitMask  <- bits$bitMask
        }
        
        if(is.null(bitShift) | is.null(bitMask)) 
          stop("Could not extract 'bits' for weighting from this product. ", 
               "Use '?makeWeights' function to generate weights manually!")

        wtu <- makeWeights(wtu, bitShift = bitShift, bitMask = bitMask, threshold = threshold, decodeOnly = FALSE)
      }
      
      wtu <- t(wtu)
      set0[wtu==0] <- TRUE
      set0[is.na(wtu)] <- TRUE
      
    ## else if 'VI_Quality' is not supplied, then weight = 1:  
    } else {
      wtu <- matrix(1, nrow = mtrdim[1], ncol = mtrdim[2])
    }
    
    if (inherits(t, "Raster")) {
      inTu <- raster::getValues(t, row = tr$row[l], nrows = tr$nrows[l])
      inTu <- t(inTu)
      
      set0[is.na(inTu)] <- TRUE
      set0[inTu <= 0] <- TRUE
      
      t0 <- min(timeInfo$inDoys[1]) - 1
      
      if (!collapse) {
        inTu <- t(repDoy(t(inTu), layerDate = timeInfo, bias = -t0))
      }
      
      inTu[set0] <- 0
      
    } else {
      if (collapse) {
        inTu <- matrix(timeInfo$inDoys,nrow=length(timeInfo$inDoys),ncol=mtrdim[2])
      } else {       
        inTu <- matrix(timeInfo$inSeq,nrow=length(timeInfo$inSeq),ncol=mtrdim[2])
      }
    }
    
    # the entire info to use or not a pix is in "wtu"
    wtu[set0] <- 0
    val[set0] <- 0    
    
    out <- matrix(NA, nrow = length(fitt), ncol = mtrdim[2])
    
    if (!is.null(outlierThreshold)) {
      kickOutlier <- function(vals, weights, lambda, threshold) {
        fTS <- ptw::whit2(vals, w = weights, lambda = lambda)
        weights[weights==1][abs(vals[weights==1]-fTS[weights==1]) > threshold] <- 0            
        return(weights)    
      }
    } else {
      # if is.null(outlierThreshold) generate a fake function to avoid a per pixel "if"
      kickOutlier <- function(vals, weights, lambda, threshold) {
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
        fTS <- ptw::whit2(valVec,w=wtVec,lambda=lambda)
        valVec[valVec < fTS] <- fTS[valVec < fTS]
      }
      out[,u] <- fTS[fitt]
      #lines(fTS,col=2)
    }
    options(warn=win$warn)
    out[,colSums(abs(out))==0] <- NA
    return(t(out))
  }
  
  for (i in seq_along(tr$row)) {    
    res <- clFun(i)
    
    if (doround) 
      res <- round(res)

    b <- writeValuesMODIS(b, res, tr$row[i], timeInfo, collapse, outputAs)
  }
  
  writeStopMODIS(b,timeInfo,outputAs,collapse)
  return(raster::stack(b))
}


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

