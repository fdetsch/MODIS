# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Licence GPL v3
 
# TODO:
# improvement of automatic sensor detection 
# make a class modisproduct
    
getProduct <- function(x=NULL,quiet=FALSE) 
{    

#load(system.file("external", "MODIS_Products.RData", package="MODIS"))

    if (is.null(x))
    { # if x isn't provided, return table of supported files.
        products <- as.data.frame(MODIS_Products[c("SENSOR", "PRODUCT", "TOPIC", "PLATFORM","TYPE", "RES", "TEMP_RES")])
        return(products[order(products$PRODUCT),])
    }

    if (is.list(x) && names(x) %in% c("request", "PRODUCT", "TOPIC", "DATE", "TILE", "TILEV", "TILEH", "CCC", "PROCESSINGDATE", "FORMAT", "SENSOR", "PLATFORM", "PF1", "PF2", "TOPIC", "TYPE", "RES", "TEMP_RES", "INTERNALSEPARATOR")) 
    {
        # if TRUE than it is a result from a getProduct() call. A good idea would be to have a CLASS for it!
        return(x)
    }
    
    ## moody but seams to work!!
    inbase  <- basename(x) # if x is a filename(+path) remove the path
    if (substring(inbase,nchar(inbase)-2, nchar(inbase)) %in% c("hdf","xml","tif",".gz","tar","zip")) 
    {
        isFile <- TRUE
        intSepTest <- c("\\.","_")[which(c(length(strsplit(inbase, "\\.")[[1]]),length(strsplit(inbase, "_")[[1]]))==max(c(length(strsplit(inbase, "\\.")[[1]]),length(strsplit(inbase, "_")[[1]]))))]
        product  <- strsplit(inbase,intSepTest)[[1]]
    } else 
    {
        isFile <- FALSE
        product <- inbase
    }
    
    product <- product[1]
    pattern <- sub(pattern="MXD", replacement="M.D", x=product, ignore.case=TRUE) # make a regEx out of "x"
    info    <- listPather(MODIS_Products,grep(pattern=pattern,x=MODIS_Products$PRODUCT,ignore.case=TRUE))

    if(length(info$PRODUCT)==0)
    {
        cat("No product found with the name ",inbase," try 'getProduct()' to list available products.\n",sep = "")
        return(NULL)
    }
    if (info$SENSOR[1]=="MODIS") 
    {
        info$PRODUCT <- toupper(info$PRODUCT)
    }
    
    if (isFile)
    { # in this case it must be a filename
        fname <- unlist(strsplit(inbase,info$INTERNALSEPARATOR[1]))
        fname <- unlist(strsplit(fname,"\\."))

        if (info$SENSOR[1] == "MODIS") 
        { # file TEST
            
            if (info$TYPE == "Tile") 
            { # file check.
             
                Tpat    <- "h[0-3][0-9]v[0-1][0-9]"
                isok <- all((grep(fname[2],pattern=Tpat)) + (substr(fname[2],1,1) == "A") + (fname[6]=="hdf") + (length(fname)==6))
            
            } else if (info$TYPE == "CMG") 
            {
            
                isok <- all((substr(fname[2],1,1) == "A") + (fname[5]=="hdf") + (length(fname)==5))
            
            } else if (info$TYPE == "Swath")
            {
                isok <- all((substr(fname[2],1,1) == "A") + (fname[6]=="hdf") + (length(fname)==6))
           
            } else 
            {
                isok <- FALSE
            }
            
            if (!isok)
            {   
            
                stop("Check filename:", inbase,"\nIt seams to be not supported...if it should please send some feedback! matteo.mattiuzzi@boku.ac.at") 
            
            }
               
            PD <- substr(info$PRODUCT[1], 4, nchar(as.character(info$PRODUCT[1])))
              
            if (info$TYPE=="Tile") 
            {
                names(fname) <- c("PRODUCT","DATE","TILE","CCC","PROCESSINGDATE","FORMAT")
            
            } else if (info$TYPE=="CMG") 
            {
                names(fname) <- c("PRODUCT","DATE","CCC","PROCESSINGDATE","FORMAT")
            
            } else if (info$TYPE=="Swath") 
            { 
                names(fname) <- c("PRODUCT","DATE","TIME","CCC","PROCESSINGDATE","FORMAT")
           
            } else 
            {
                stop("Not a 'Tile', 'CMG' or 'Swath'! Product not supported. See: 'getProduct()'!")
            }
            request <- x
            names(request) <- "request"
            result <- c(request,fname,info)
            result <- result[!duplicated(names(result))]
            result <- as.list(sapply(result,function(x)as.character(x)))

            return(invisible(result))  
          
      } else if (info$SENSOR[1] %in% c("MERIS","C-Band-RADAR")) 
      {
          return(
              invisible(
                  list(request = as.character(info$PRODUCT), PF1 = NULL, 
                  PF2 = NULL, PD = NULL, PLATFORM = as.character(info$PLATFORM),
                  TYPE = as.character(info$TYPE), PRODUCT = as.character(info$PRODUCT),
                  SENSOR = as.character(info$SENSOR), SOURCE=info$SOURCE)
              )
          )
      }         
    } else  # if not a file
    {
        if (!quiet) 
        {
           for (i in seq_along(info$PRODUCT)) 
           {
               cat(paste(info$PRODUCT[i],'the',info$TEMP_RES[i],info$TYPE[i], info$TOPIC[i],'product from',info$SENSOR[i], info$PLATFORM[i],'with a ground resolution of', info$RES[i],'\n', sep = " "))
           }
        }

        if (info$SENSOR[1] == "MODIS") 
        {    
            PD <- substr(info$PRODUCT, 4, nchar(as.character(info$PRODUCT)))
            
            return(
                invisible(
                    list(request = inbase, PF1 = as.character(info$PF1),
                    PF2 = as.character(info$PF2), PD = PD, PLATFORM = as.character(info$PLATFORM),
                    TYPE = as.character(info$TYPE), PRODUCT = as.character(info$PRODUCT),
                    SENSOR = as.character(info$SENSOR), SOURCE=info$SOURCE)
                )
            )
    
        } else if (info$SENSOR[1] %in% c("MERIS","C-Band-RADAR"))
        {
            return(
                invisible(
                    list(request = as.character(info$PRODUCT), PF1 = NULL,
                    PF2 = NULL, PD = NULL, PLATFORM = as.character(info$PLATFORM),
                    TYPE = as.character(info$TYPE), PRODUCT = as.character(info$PRODUCT),
                    SENSOR = as.character(info$SENSOR), SOURCE=info$SOURCE)
                )       
            )
        }         
    }
}



