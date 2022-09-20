#' Extract Bit-Encoded Information and create Weights Raster
#' 
#' @description 
#' This function applies [bitops::bitAnd()] and [bitops::bitShiftR()] to convert
#' bit-encoded information. It is also possible to convert this information to a
#' scale from 0 to 1 in order to use it as weighting information in functions 
#' like [whittaker.raster()] or [smooth.spline.raster()].
#' 
#' @param x `matrix`, vector or `Raster*` object.
#' @param X `Raster*` object.
#' @param bitShift `integer`. Bit starting point, see Examples and 
#'   [detectBitInfo()].
#' @param bitMask `integer`. Bit mask size, see Examples and [detectBitInfo()].
#' @param threshold `integer`. Threshold for valid quality.
#' @param filename `character` passed to [raster::writeRaster()]. If not 
#'   specified, output is written to a temporary file.
#' @param decodeOnly `logical`. If `FALSE` (default), convert bits to weights 
#'   from 0 (not used) to 1 (best quality). If `TRUE`, only extract selected 
#'   bits and convert to decimal system.
#' @param keep If `NULL` (default), bits are only encoded, else an `integer` 
#'   vector of values you want to keep (becomes `TRUE`), the rest becomes `NA`. 
#'   See Examples.
#' @param datatype `character`. Default `"INT1U"` used for `Raster*` object. 
#'   Output datatype, see [raster::writeRaster()].
#' @param NAflag `integer`. Default `255` used for `Raster*` object. Set 
#'   specific NA value, see [raster::writeRaster()].
#' @param ... Other arguments passed to [raster::writeRaster()]. 
#'    
#' 
#' @return 
#' A `Raster*` object.
#' 
#' @note 
#' [makeWeights()] and [extractBits()] are identical with the only difference 
#' that [makeWeights()] does additionally convert the data into weighting 
#' information.
#' 
#' @seealso 
#' [detectBitInfo()].
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' \dontrun{
#' 
#' # example MOD13Q1 see MODIS Vegetation Index User's Guide (MOD13 Series; 
#' # https://lpdaac.usgs.gov/sites/default/files/public/product_documentation/mod13_user_guide.pdf)
#' # enter in Layers
#' # See in TABLE 5: Descriptions of the VI Quality Assessment Science Data Sets (QA SDS).
#' # column 1 (bit) row 2 VI usefulness 
#' bitShift = 2
#' bitMask = 15 # ('15' is the decimal of the binary '1111')  
#' # or try to use
#' detectBitInfo("MOD13Q1") # not all products are available!
#' viu <- detectBitInfo("MOD13Q1","VI usefulness") # not all products are available!
#' viu
#' 
#' # simulate bit info
#' bit <- round(runif(10*10,1,65536))
#' 
#' # extract from vector
#' makeWeights(bit,bitShift,bitMask,decodeOnly=TRUE)
#' # the same as
#' extractBits(bit,bitShift,bitMask)
#' 
#' # create a Raster object
#' VIqual <- raster(ncol=10,nrow=10)
#' VIqual[] <- bit
#' 
#' # extract from Raster object
#' a <- makeWeights(VIqual,bitShift,bitMask,decodeOnly=TRUE)
#' 
#' # linear conversion of 0 (0000) to 15 (1111) to 1 fo 0 
#' b <- makeWeights(VIqual,bitShift,bitMask,decodeOnly=FALSE)
#' 
#' threshold=6 # every thing < threshold becomes a weight = 0
#' c <- makeWeights(VIqual,bitShift,bitMask,threshold,decodeOnly=FALSE)
#' 
#' res <- round(cbind(a[],b[],c[]),2)
#' colnames(res) <- c("ORIG","Weight","WeightThreshold") 
#' res
#' 
#' #####
#' # water mask
#' tif = runGdal(product="MOD13A2",begin="2009001",end="2009001", extent=extent(c(-9,-3 ,54,58)),
#'               SDSstring="001",job="delme") # 6.4 MB
#' x <- raster(unlist(tif))
#' 
#' res1 <- maskWater(x)
#' plot(res1)
#' 
#' res2 <- maskWater(x,keep=1) # 1 = Land (nothing else)
#' x11()
#' plot(res2)
#' 
#' # Land (Nothing else but land) + Ocean coastlines and lake shorelines + shallow inland Water,
#' # the rest becomes NA
#' x11()
#' res3 <- maskWater(x,keep=c(1,2,3)) 
#' plot(res3)
#' 
#' ###############
#' 
#' # as on Linux you can read HDF4 directly you can also do:
#' if(.Platform$OS.type=="unix")
#' {
#'   x <- getHdf(HdfName="MOD13A2.A2009001.h17v03.005.2009020044109.hdf", wait=0) # 6.4 MB
#'   
#'   detectBitInfo(x) # just info
#'   getSds(x) # just info
#'   
#'   x <- getSds(x)$SDS4gdal[3] # you need 'VI Quality'
#'   x  <- raster(x) 
#'   # plot(x)
#'   # ex <- drawExtent()
#'   ex <- extent(c(-580779,-200911,5974929,6529959))
#'   x  <- crop(x,ex) # just for speed-up
#'   
#'   res1 <- maskWater(x)
#'   plot(res1)
#'   
#'   res2 <- maskWater(x,keep=1) # 1 = Land (Nothing else but land), the rest becomes NA
#'   x11()
#'   plot(res2)
#'   
#'   # Land (Nothing else but land) + Ocean coastlines and lake shorelines + shallow inland Water,
#'   # the rest becomes NA
#'   res3 <- maskWater(x,keep=c(1,2,3))
#'   x11()
#'   plot(res3)
#' }
#' }
#' 
#' @describeIn makeWeights Extract bit-encoded information from `Raster*` file
#' @aliases extractBits
#' @export extractBits
extractBits <- function(x, bitShift=2, bitMask=15, filename='', datatype='INT1U', NAflag=255,...)
{
  if (inherits(x,"Raster"))
  {
    stopifnot(inherits(x,"RasterLayer"))
    
    opts <- combineOptions(...)
    
    if(!inMemory(x))
    {
      naok <- validNa(filename(x))[[1]]
      na   <- getNa(filename(x))[[1]]
    } else
    { # switch off recoding
      naok <- TRUE
      na   <- NULL
    }
    
    out <- raster(x)
    out <- writeStart(out, filename=filename, datatype=datatype, NAflag=NAflag,...)

    minrows <- max(floor(opts$cellchunk/ncol(out)),1)
    tr      <- blockSize(out,minrows = minrows)

    for (i in 1:tr$n) 
    { # i=1
      # cat(i,'/',tr$n,'\n')
        v  <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])

        if(!isTRUE(naok))
        {
          v[is.na(v)] <- na   
        }
        v <- as.integer(v)
        
        # decode bits
        v <- bitAnd(bitShiftR(v, bitShift ), bitMask)
        out <- writeValues(out, v, tr$row[i])
      }
    out <- writeStop(out)

    return(out)
  } else
  {
    ve <- dim(x)
    
    # decode bits
    x <- bitAnd(bitShiftR(x, bitShift ), bitMask)

    if (!is.null(ve))
    {
        x <- matrix(x,ncol=ve[2],nrow=ve[1],byrow=FALSE)
    }
    return(x)
  }
}    


#' @export makeWeights
#' @name makeWeights
makeWeights <- function(x, bitShift=2, bitMask=15, threshold=NULL, decodeOnly=FALSE, filename='', datatype='INT1U', NAflag=255,...)
  {
    if (inherits(x,"Raster"))
    {
      stopifnot(inherits(x,"RasterLayer"))
      
      opts <- combineOptions(...)
      if(!inMemory(x))
      {
        naok <- validNa(filename(x))[[1]]
        na   <- getNa(filename(x))[[1]]
      } else
      { # switch off recoding
        naok <- TRUE
        na   <- NULL
      }
      
      out <- raster(x)
      out <- writeStart(out, filename=filename, datatype=datatype, NAflag=NAflag,...)
      
      minrows <- max(floor(opts$cellchunk/ncol(out)),1)
      tr      <- blockSize(out,minrows = minrows)
      
      for (i in 1:tr$n) 
      { # i=1
        # cat(i,'/',tr$n,'\n')
        v  <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])

        if(!isTRUE(naok))
        {
          v[is.na(v)] <- na   
        }
        
        v <- as.integer(v)
        v <- bitAnd(bitShiftR(v, bitShift ), bitMask)
        
        if (!is.null(threshold))
        {
          v[v > threshold] <- bitMask
        }
        
        if (!decodeOnly)
        {
          # turn up side down and scale bits for weighting
          v <- (-1 * (v - bitMask))/bitMask
          v[v > 1] <- 1
          v[v < 0] <- 0 # this should not happen!?   
        }
        
        # decode bits
        out <- writeValues(out, v, tr$row[i])
      }
      out <- writeStop(out)
      
      return(out)
    } else
    {
      ve <- dim(x)
      
      # decode bits
      v <- bitAnd(bitShiftR(x, bitShift ), bitMask)
      
      if (!is.null(threshold))
      {
        v[v > threshold] <- bitMask
      }
      
      if (!decodeOnly)
      {
        # turn up side down and scale bits for weighting
        v <- (-1 * (v - bitMask))/bitMask
        v[v > 1] <- 1
        v[v < 0] <- 0      
      }
      
      if (!is.null(ve))
      {
        v <- matrix(v,ncol=ve[2],nrow=ve[1],byrow=FALSE)
      }
      return(v)
    }
  }    

 
### maskWater (experimental)
#' @describeIn makeWeights Masks water (additional information required)
#' @aliases maskWater
#' @export maskWater
maskWater <- function(X, bitShift=NULL, bitMask = NULL, keep = NULL, datatype="INT1U", NAflag=255,...)
{
    if (!inherits(X,"Raster"))
    {
        stop("'maskWater' requires a raster* object")
    }
    
    if (is.null(bitShift) | is.null(bitMask))
    {
        cat("'bitShift', 'bitMask' not defined, trying to autodetect 'Land/Water Flag'!...\n")
        fname    <- basename(names(X)[1])        
        prodinfo <- strsplit(fname,"\\.")[[1]][1]

        bits     <- detectBitInfo(prodinfo, what='Land/Water Flag',warn=FALSE)
        bitShift <- bits$bitShift
        bitMask  <- bits$bitMask
        
        if(is.null(bits))
        {
            stop(paste0("No 'Land/Water Flag' found, please set 'bitShift', 'bitMask' manualy. See: https://lpdaac.usgs.gov/products/modis_products_table/",tolower(prodinfo)))
        } else 
        {
            message("Ok 'Land/Water Flag' found!")
        }
    }
    
    result <- extractBits(X, bitShift = bitShift, bitMask = bitMask, datatype=datatype, NAflag=NAflag,...)

    if (!is.null(keep))
    {
        eval(parse(text=paste("result <- result %in% ", paste0("c(",paste(keep,collapse=","),")"))))  
        NAvalue(result) <- 0  
    }
return(result)
}
