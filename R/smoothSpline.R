#' Filter Time Series Imagery with a Cubic Spline
#' 
#' @description 
#' This function uses the [stats::smooth.spline()] function to filter a 
#' vegetation index time series of satellite data.
#' 
#' @param x `RasterBrick` (or `RasterStack`) or `character` vector of file 
#'   names, sorted 'Vegetation index'.
#' @param w `RasterBrick` (or `RasterStack`) with weighting information, e.g. 
#'   derived from [makeWeights()].
#' @param t In case of MODIS composite, the corresponding 
#'   'composite_day_of_the_year' `RasterBrick` (or `RasterStack`).
#' @param groupYears `logical`. If `TRUE` (default), output files are grouped by
#'   years. 
#' @param timeInfo Result from [orgTime()].
#' @param df `numeric`, yearly degree of freedom value passed to 
#'   [stats::smooth.spline()]. If set as `character` (i.e., `df = "6"`), it is 
#'   not adapted to the time series length but used as a fixed value (see 
#'   Details).
#' @param outDirPath Output path, defaults to the current working directory.
#' @param ... Arguments passed to [raster::writeRaster()]. Note that 'filename' 
#'   is created automatically.
#' 
#' @return 
#' The filtered data and a text file with the dates of the output layers.
#'
#' @details 
#' `numeric` values of 'df' (e.g., `df = 6`) are treated as yearly degrees of 
#' freedom. Here, the length of the input time series is not relevant since `df`
#' is adapted to it with: `df * ('length of _input_ timeserie in days' / 365)`. 
#' The input length can differ from the output because of the 'pillow' argument 
#' in [orgTime()].
#' 
#' `character` values of 'df' (e.g., `df = "6"`), on the other hand, are not 
#' adopted to the length of the input time series.
#'
#' @details 
#' Currently tested on MODIS and Landsat data. With M*D13 data, it is also 
#' possible to use the 'composite_day_of_the_year' layer and the 'VI_Quality' 
#' layer.
#' 
#' @seealso 
#' [whittaker.raster()], [raster::raster()].
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' \dontrun{
#' # The full capacity of the following functions is currently available only 
#' # with M*D13 data.
#' # !! The function is very new, double check the result!!
#' 
#' # You need to extract the: 'vegetation index', 'VI_Quality layer', 
#' # and 'composite day of the year' layer.
#' # runGdal(product="MOD13A2",begin="2004340",extent="sicily",end="2006070",
#' # job="fullCapa",SDSstring="101000000010")
#' # Afterward extract it to: 
#' options("MODIS_outDirPath")
#' 
#' # the only obligatory dataset is "x" (vegetatino index), get the 'vi' data on
#' # the source directory: 
#' path <- paste0(options("MODIS_outDirPath"),"/fullCapa")
#' vi <- preStack(path=path, pattern="*_NDVI.tif$")
#' 
#' # `orgTime()` detects timing information of the input data and generates 
#' # based on the arguments the output date information. For spline functions 
#' # (in general) the start and end of the time series is always problematic. 
#' # So there is the argument "pillow" (default 75 days) that adds
#' # (if available) some more layers on the two endings.
#' 
#' timeInfo <- orgTime(vi,nDays=16,begin="2005001",end="2005365",pillow=40)
#' 
#' # now re-run "preStack" with two differences, 'files' (output of the first 
#' # `preStack()` call) and the 'timeInfo'.
#' # Here only the data needed for the filtering is extracted:
#' vi <- preStack(files=vi,timeInfo=timeInfo)
#' 
#' smooth.spline.raster(x=vi,timeInfo=timeInfo)
#' 
#' # Filter with weighting and time information:
#' # if the files are M*D13 you can use also use quality layers and the 
#' # composite day of the year:
#' w <- stack(preStack(path=path, pattern="*_VI_Quality.tif$", timeInfo=timeInfo))
#' w <- makeWeights(w,bitShift=2,bitMask=15,threshold=6)
#' # you can also pass only the names
#' t <- preStack(path=path, pattern="*_composite_day_of_the_year.tif$", timeInfo=timeInfo)
#' 
#' smooth.spline.raster(x=vi,w=w,t=t,timeInfo=timeInfo)
#' }
#' 
#' @export smooth.spline.raster
#' @name smooth.spline.raster
smooth.spline.raster <- function(x, w=NULL, t=NULL, groupYears=TRUE, timeInfo = orgTime(x), df = 6,outDirPath = "./",...)
{
    
    opt <- combineOptions(...)
    
    dir.create(opt$outDirPath,showWarnings=FALSE)
    outDirPath <- normalizePath(opt$outDirPath, winslash = "/", mustWork = TRUE)

    outDirPath <- setPath(outDirPath)
    # bitShift   <- opts$bitShift
    # bitMask    <- opts$bitMask
    # threshold  <- opts$threshold
    
    dataFormat <- opt$dataFormat
    rasterOut  <- toupper(writeFormats())

    if(!toupper(dataFormat) %in% rasterOut[,"name"]) {
      stop("Unknown or unsupported data format: '", dataFormat, "'. Please run 
         raster::writeFormats() (column 'name') for supported file types.\n")
    }
    
    if (!inherits(x, "Raster")) 
      x <- raster::stack(x)

    if (!inherits(w, "Raster") & !is.null(w)) 
      w <- raster::stack(w)

    if (!inherits(t, "Raster") & !is.null(t)) 
      t <- raster::stack(t)

    tsLength <- as.numeric(max(timeInfo$inputLayerDates) - (min(timeInfo$inputLayerDates)-1)) 
    tsLayers <- length(unique(timeInfo$inputLayerDates))

    indf <- df    
    if (is.character(df))
    {
        cat("Using fixed 'df':",df,"\n")
        nameDf <- "FixedDf"
    } else
    {
        df   <- df*(tsLength/365)
        cat("Yearly 'df' is:",indf,"\nNow changed with df*('length of input data period in days'/365) to:",df,"\n")
        nameDf <- "YearlyDf"
    }
    df <- as.numeric(df)
    
    # TEMP
    
    b <- list()
    if (groupYears)
    {
        for (a in seq_along(unique(format(timeInfo$outputLayerDates,"%Y"))))
        {
            y <- unique(format(timeInfo$outputLayerDates,"%Y"))[a]
            b[[a]] <- brick(raster(x),nl=as.integer(sum(format(timeInfo$outputLayerDates,"%Y")==y)), values=FALSE)
            b[[a]] <- writeStart(b[[a]], filename=paste0(opt$outDirPath,"/NDVI_",nameDf,indf,"_year",y)
                                 , format = dataFormat)
        }
    
    } else 
    {
        b[[1]] <- brick(raster(x),nl=as.integer(length(timeInfo$outSeq)), values=FALSE)  
        b[[1]] <- writeStart(b[[1]], filename=paste0(opt$outDirPath,"/NDVI_",nameDf,indf,"_fullPeriod")
                             , format = dataFormat)
    }
        
    tr <- blockSize(x)

    cat("Data is in, start processing!\n")
###############################
# clusterFuns: 

clFun <- function(l)
{
    minval      <- -2000
    
    val    <- getValues(x, row=tr$row[l], nrows=tr$nrows[l])
    mtrdim <- dim(val)
    set0   <- val <= minval # M.D13 specific!
    set0[is.na(val)] <- TRUE
    set0[rowSums(val,na.rm=TRUE)==0] <- TRUE
    
    if (!is.null(w))
    {
        wtu <- getValues(w, row=tr$row[l], nrows=tr$nrows[l])
        
        # is it a weight info?
        if(max(wtu) > 1)
        {
            bits <- detectBitInfo(vi,"VI usefulness",warn=FALSE)
            
            if(is.null(bits))
            {
                stop("Could not extract 'bits' for weighting from this product. Use 'makeWeights' function to generate weightings manualy!")
            }
            wtu  <- makeWeights(wtu, bitShift = bits$bitShift, bitMask = bits$bitMask, decodeOnly = TRUE)
        }
        set0[wtu==0] <- TRUE

    } else
    {
        wtu <- matrix(1,nrow=mtrdim[1],ncol=mtrdim[2])
    }
     
    if (inherits(t,"Raster"))
    {
        inTu  <- getValues(t, row=tr$row[l], nrows=tr$nrows[l])
        inTu  <- repDoy(inTu,timeInfo,bias=timeInfo$inSeq[1]-1)
        set0[is.na(inTu)] <- TRUE
        inTu[set0] <- 0
    } else 
    {
        inTu <- matrix(timeInfo$inSeq,nrow=mtrdim[1],ncol=mtrdim[2],byrow=TRUE)
    }

    wtu[set0] <- 0
    val[set0] <- 0    
     
    r <- smooth.splineMtr(vali=val,wti=wtu,inTi=inTu,timeInfo=timeInfo,df=df)
    r[rowSums(abs(r))==0,] <- NA

return(r)
}

    for ( i in seq_along(tr$row) )
    {    
      res <- clFun(i)
      res <- round(res)
      
      if (groupYears)
      {
        for (a in seq_along(unique(format(timeInfo$outputLayerDates,"%Y"))))
        {
          y <- unique(format(timeInfo$outputLayerDates,"%Y"))[a]
          b[[a]] <- writeValues(b[[a]], res[,format(timeInfo$outputLayerDates,"%Y")==y], tr$row[i])
        }   
      } else 
      {
        b[[1]]  <- writeValues(b[[1]], res, tr$row[i])
      }
    }       
    
    for (a in seq_along(b))
    {    
        b[[a]] <- writeStop(b[[a]])
        if (groupYears)
        {
            y <- unique(format(timeInfo$outputLayerDates,"%Y"))[a]
            write.table(x=timeInfo$outputLayerDates[format(timeInfo$outputLayerDates,"%Y")==y], file=paste(opt$outDirPath,"/LayerDates_NDVI_",nameDf,indf,"_year",y,sep=""), row.names=FALSE, col.names=FALSE)
        } else
        {
            write.table(x=timeInfo$outputLayerDates, file=paste(opt$outDirPath,"/LayerDates_NDVI_",nameDf,indf,"fullPeriod",sep=""), col.names=FALSE, row.names=FALSE)
        }
    }

return(NULL)
}

# vali=val;wti=wtu;inTi=inTu;timeInfo=timeInfo;df=df
smooth.splineMtr <- function(vali,wti=NULL,inTi=NULL,timeInfo=NULL,df=NULL)
{
    vali <- t(vali)
    
    yRow <- nrow(vali)
    yCol <- ncol(vali)

    if(is.null(wti))
    {
        wti <- matrix(1,nrow=yRow,ncol=yCol)
    } else {
        wti <- as.matrix(wti)
        wti <- t(wti)
    }
                    
    if(is.null(inTi))
    {
        inTi <- matrix(1:yRow,ncol=yCol,nrow=yRow)
    } else 
    {
        inTi <- as.matrix(inTi)
        # if inT is a fixed vector (i.e.: from filename of Landsat of length nrow(x) (==nlayer) create a matrix with 1:nlayer for each col.
        if(ncol(inTi)==1)
        {
            inTi <- matrix(inTi[,1],ncol=yCol,nrow=yRow)            
        } else {
            inTi <- t(inTi)
        }
    }
    
    # generate output matrix    
    if (is.null(timeInfo))
    {
        outTi <- inTi
        out   <- matrix(NA, nrow=nrow(inTi), ncol=yCol)
    } else {
        outTi <- as.matrix(timeInfo$outSeq)
        if (ncol(outTi)==1)
        {
            outTi <- matrix(outTi, nrow=length(outTi), ncol=yCol)            
        }
        out <- matrix(NA, nrow=nrow(outTi), ncol=yCol)
    }
        
    # minimum "minVal" input values for filtering 
    Cvec <- (colSums(wti>0) > df)
    Cvec <- (1:yCol)[Cvec]

    for (u in Cvec)
    {
        s       <- smooth.spline(y=vali[,u], x=inTi[,u], w=wti[,u], df=df, tol=1)
        out[,u] <- predict(s, outTi[,u])$y
    }

return(t(out))
}
   
