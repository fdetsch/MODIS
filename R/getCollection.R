#' Get Available Collections of MODIS Product(s)
#' 
#' @description 
#' Checks and retrieves available MODIS collection(s) for a given product.
#' 
#' @param product \code{character}. MODIS grid product to check for existing 
#' collections, see \code{\link{getProduct}}.
#' @param collection \code{character} or \code{integer}. If provided, the 
#' function only checks if the specified collection exists and returns the 
#' collection number formatted based on the \code{as} parameter or \code{FALSE} 
#' if it doesn't exists. The check is performed on the primary data source, 
#' 'LP DAAC' \url{https://lpdaac.usgs.gov/}.
#' @param newest \code{logical}. If \code{TRUE} (default), return only the 
#' newest collection, else return all available collections.
#' @param forceCheck \code{logical}, defaults to \code{FALSE}. If \code{TRUE}, 
#' connect to the 'LP DAAC' FTP server and get available collections, of which 
#' an updated version is permanently stored in 
#' \code{MODIS:::combineOptions()$auxPath}.
#' @param as \code{character}, defaults to \code{'character'} which returns the 
#' typical 3-digit collection number (i.e., \code{"005"}). \code{as = 'numeric'} 
#' returns the result as \code{numeric} (i.e., \code{5}).
#' @param quiet \code{logical}, defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{MODIS:::combineOptions}.
#' 
#' @return 
#' A 3-digit \code{character} or \code{numeric} object (depending on 'as') or, 
#' if \code{length(product) > 1}, a \code{list} of such objects with each slot 
#' corresponding to the collection available for a certain product. 
#' Additionally, a text file in a hidden folder located in 
#' \code{getOption("MODIS_localArcPath")} as database for future calls. If 
#' 'collection' is provided, only the (formatted) collection (or \code{FALSE} if 
#' it could not be found) is returned.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @seealso 
#' \code{\link{getProduct}}.
#' 
#' @examples 
#' \dontrun{
#' 
#' # update or get collections for MOD11C3 and MYD11C3
#' getCollection(product="M.D11C3")
#' getCollection(product="M.D11C3",newest=FALSE)
#' 
#' getCollection(product="M.D11C3",collection=3)
#' getCollection(product="M.D11C3",collection=41)
#' getCollection(product="M.D11C3",collection="041")
#' getCollection(product="M.D11C3",forceCheck=TRUE)
#' }
#' 
#' @export getCollection
#' @name getCollection
getCollection <- function(product,collection=NULL,newest=TRUE,forceCheck=FALSE,as="character",quiet=TRUE, ...)
{
    opts <- combineOptions(...)

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
    
    ## if 'collections' dataset does not exist in opts$auxPath, copy it from 
    ## 'inst/external', then import data
    dir_aux <- opts$auxPath
    if (!dir.exists(dir_aux)) dir.create(dir_aux)
    
    fls_col <- paste0(dir_aux, "/collections.RData")
    
    if (!file.exists(fls_col))
      invisible(
        file.copy(system.file("external", "collections.RData", package = "MODIS"), 
                  fls_col)
      )

    load(fls_col)
    
    MODIScollection <- MODIScollection[, grep(colnames(MODIScollection), 
                                              pattern = "M.D")]

    if (forceCheck | sum(!productN$PRODUCT %in% colnames(MODIScollection))>0) 
    {
      sturheit <- stubborn(level=opts$stubbornness)
      
      load(system.file("external", "MODIS_FTPinfo.RData", package = "MODIS"))
      
      for (i in seq_along(unique(productN$PF1))) 
      {	
        ## retrieve ftp server address based on product source information
        server <- unlist(productN$SOURCE)
        
        ftp_id <- sapply(MODIS_FTPinfo, function(i) i$name %in% server)
        ftp_id <- which(ftp_id)[1]
        
        ftp <- paste0(MODIS_FTPinfo[[ftp_id]]$basepath,"/",unique(productN$PF1)[i],"/")
        cat("Updating collections from", server[1], "for platform:",unique(productN$PLATFORM)[i],"\n")
        
        if(exists("dirs")) 
        {
          suppressWarnings(rm(dirs))
        }
        for (g in 1:sturheit)
        {
          try(dirs <- filesUrl(ftp))
          if(exists("dirs"))
          {
            if(all(dirs != FALSE))
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
          ## if 'product' is hosted on NTSG server, remove non-product folders 
          ## and files
          if (productN$SOURCE[[1]][1] == "NTSG") {
            dirs <- dirs[grep("^MOD16", dirs)]
            
            # remove .pdf files  
            if (length(grep(".pdf$", dirs)) > 0)
              dirs <- dirs[-grep(".pdf$", dirs)]
            
            # remove '_MERRAGMAO' extension
            dirs <- dirs[grep("_MERRAGMAO", dirs)]
            dirs <- gsub("_MERRAGMAO", "", dirs)
          }
          
          ## information about products and collections    			  
          ls_prod_col <- sapply(dirs, function(x) {strsplit(x, "\\.")})
          prod <- sapply(ls_prod_col, "[[", 1)
          coll <- sapply(ls_prod_col, "[[", 2)
          
          mtr  <- cbind(prod,coll)
          mtr  <- tapply(INDEX=mtr[,1],X=mtr[,2],function(x){x})
          
          maxrow <- max(nrow(MODIScollection),sapply(mtr,function(x)length(x)))
          
          basemtr <- matrix(NA,ncol=nrow(mtr), nrow = maxrow)
          colnames(basemtr) <- names(mtr)
          
          for(u in 1:ncol(basemtr)) 
          {
            basemtr[1:length(mtr[[u]]),u] <- mtr[[u]]
          }
          
          ## if new collections are available, 
          ## add additional rows to 'MODIScollection'
          if (nrow(MODIScollection) < maxrow & nrow(MODIScollection) > 0) 
          {
            new_rows <- matrix(data = NA, nrow = maxrow-nrow(MODIScollection), 
                               ncol = ncol(MODIScollection))
            new_rows <- data.frame(new_rows)
            names(new_rows) <- names(MODIScollection)
            
            MODIScollection <- rbind(MODIScollection, new_rows)
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
    
    #write.table(MODIScollection,file.path(opts$auxPath,"collections",fsep="/"))
    ind <- which(colnames(MODIScollection)%in%productN$PRODUCT)

    if(length(ind)==1)
    {
	    res <- list(MODIScollection[,ind])
	    names(res) <- colnames(MODIScollection)[ind]
    } else if (length(ind)>1) 
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
    
    ## make changes permanent by saving updated 'collections' dataset in 
    ## opts$auxPath
    save(MODIScollection, file = fls_col)
    
return(res)
}

