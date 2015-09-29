# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : August 2011
# Licence GPL v3


getCollection <- function(product,collection=NULL,newest=TRUE,forceCheck=FALSE,as="character",quiet=TRUE)
{
    opts <- combineOptions()

    ####
    # checks for product
    if (missing(product))
    {
        stop("Please provide a valid product")
    }
    productN <- getProduct(x=product,quiet=TRUE)
    if (is.null(productN)) 
    {
        stop("Unknown product")
    }
    # load aux
#    if (!file.exists(paste0(opts$auxPath,"collections.RData"))) # on the very first call use the delivered pre-updated version    
#    {
#        opts$auxPath <- setPath(opts$auxPath)
#        invisible(file.copy(file.path(find.package("MODIS"), "external","collections.RData"), file.path(opts$auxPath,"collections.RData",fsep="/")))
#        unlink(file.path(opts$auxPath,"collections.txt",fsep="/"))
#    }
#    load(paste0(opts$auxPath,"collections.RData"))
    
#    unlink(paste0(opts$auxPath,"collections.RData"))

#    if (!file.exists(paste0(opts$auxPath,"collections"))) # on the very first call use the delivered pre-updated version    
#    {
#        opts$auxPath <- MODIS:::setPath(opts$auxPath)
#        invisible(file.copy(file.path(find.package("MODIS"), "/external","collections"), paste0(opts$auxPath,"collections")))
#    }
#    MODIScollection <- read.table(paste0(opts$auxPath,"collections"))
     
    # clean file
    MODIS <- MODIScollection[,grep(colnames(MODIScollection),pattern="M.D")]
    SRTM  <- MODIScollection[,grep(colnames(MODIScollection),pattern="SRTM")]
    MODIScollection <- cbind(MODIS,SRTM)

    if (productN$SENSOR[1] !="C-Band-RADAR")
    {    
      if (forceCheck | sum(!productN$PRODUCT %in% colnames(MODIScollection))>0) 
      {
        sturheit <- stubborn(level=opts$stubbornness)
		    
    		for (i in seq_along(unique(productN$PF1))) 
    		{		
    		  ftp <- paste0(MODIS_FTPinfo$ftpstring1$basepath,"/",unique(productN$PF1)[i],"/")
    			cat("Updating collections from LPDAAC for platform:",unique(productN$PLATFORM)[i],"\n")
    
    			if(exists("dirs")) 
    			{
    			    rm(dirs)
    			}
    			for (g in 1:sturheit)
    			{
    			  try(dirs <- filesUrl(ftp))
    				if(exists("dirs"))
    				{
    				  if(dirs != FALSE)
    				  {
    				    break
    				  }
    			  }
    			} 
    
    			if (!exists("dirs")) 
    			{
    				cat("FTP is not available, using stored information from previous calls (this should be mostly fine)\n")
    			} else 
    			{
    				prod <- sapply(dirs,function(x){strsplit(x, "\\.")[[1]][1]})
    				coll <- sapply(dirs,function(x){strsplit(x, "\\.")[[1]][2]})
    		
    				mtr  <- cbind(prod,coll)
    				mtr  <- tapply(INDEX=mtr[,1],X=mtr[,2],function(x){x})
    		
    				maxrow <- max(nrow(MODIScollection),sapply(mtr,function(x)length(x)))
    				
    				basemtr <- matrix(NA,ncol=nrow(mtr), nrow = maxrow)
    				colnames(basemtr) <- names(mtr)
    		
    				for(u in 1:ncol(basemtr)) 
    				{
    					basemtr[1:length(mtr[[u]]),u] <- mtr[[u]]
    				}
    				
    				if (nrow(MODIScollection) < maxrow & nrow(MODIScollection) > 0) 
    				{
    					MODIScollection <- rbind(MODIScollection,as.data.frame(NA,nrow=(maxrow-nrow(MODIScollection)), ncol=ncol(MODIScollection)))
    				}
    					
    				if (ncol(MODIScollection)==0)
    				{ # relevant only for time
    					MODIScollection <- data.frame(basemtr) # create new
    				} else 
    				{ # or update the available one
    					indX    <- colnames(MODIScollection) %in% colnames(basemtr) 
    					MODIScollection <- cbind(MODIScollection[,!indX],basemtr)
    				}
    			}
    		}
    	}
    }
    #write.table(MODIScollection,file.path(opts$auxPath,"collections",fsep="/"))
    ind <- which(colnames(MODIScollection)%in%productN$PRODUCT)

    if(length(ind)==1)
    {
	    res <- list(MODIScollection[,ind])
	    names(res) <- colnames(MODIScollection)[ind]
    } else if (length(ind)>=1) 
    {
	    res <- as.list(MODIScollection[,ind])
    } else 
    {
	    stop("No data available, check product input?") # should not happen getProduct() should catch that before
    }

    res <- lapply(res, function(x){as.numeric(as.character(x[!is.na(x)]))})

    if (!is.null(collection)) 
    { # if collection is provided...return formatted collection or 'FALSE'
	
	    isOk <- lapply(res,function(x)
	    {
		    if (as.numeric(collection) %in% x)
		    {
				as.numeric(collection)
			} else 
			{
				FALSE		
			}
		})
	
	    if (sum(isOk==FALSE)==length(isOk)) 
	    {
		    cat("Product(s) not available in collection '",collection,"'. Try 'getCollection('",productN$request,"',newest=FALSE,forceCheck=TRUE)'\n",sep="")
	        return(invisible(isOk))
	    } else if (sum(isOk==FALSE)>0 & sum(isOk==FALSE)<length(isOk))
	    {
		    cat("Not all the products in your input are available in collection '", collection,"'. Try 'getCollection('", productN$request, "', newest=FALSE, forceCheck=TRUE)'\n", sep="")
	    }

	    res <- isOk[isOk!=FALSE]

    } else if (newest) 
    {
	    if(!quiet) {cat("No Collection specified getting the newest for",productN$PRODUCT,"\n",sep=" ")}

	    res <- lapply(res,function(x)
	    { #select the newest
		    x[order(sapply(x,function(c){		
		    s <- nchar(c)-1
		    if (s==0) 
		    {
		    	c
		    } else 
		    {
		    	c/as.numeric(paste(1,rep(0,s),sep=""))
		    }}),decreasing=TRUE)][1]
		})
    }   

    if (as=="character") 
    {
	    res <- lapply(res,function(x){sprintf("%03d",x)})	
    }
return(res)
}

