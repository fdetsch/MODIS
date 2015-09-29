# Author: Matteo Mattiuzzi, matteo.mattiuzzi@boku.ac.at
# Date : February 2012
# Licence GPL v3

# 'date' is the date of an existing file! result from getStruc() and passed as single date! For format see ?transDate

genString <- function(x, collection=NULL, date=NULL, what="images", local=TRUE, remote=TRUE, ...)
{
  product <- getProduct(x=x,quiet=TRUE)

  if(length(product$PRODUCT)>1)
  {
    warning("genString() does not support multiple products! Generating 'path' for the first product:", product$PRODUCT[1], "\n")
    product <- lapply(product,function(x){x[1]}) # take only the first argument
  }

  if(length(product$CCC)==0)
  {
    product$CCC <- getCollection(product=product$PRODUCT,collection=collection)[[1]]
  }
     
  if (!is.null(date)) 
  {
    product$DATE <- list(paste0("A",transDate(date)$beginDOY)) # generates MODIS file date format "AYYYYDDD"
  }

  opts         <- combineOptions(...)
  opts$auxPath <- setPath(opts$auxPath)
  remotePath   <- localPath <- NULL    
    
  if (is.null(product$DATE)) # if x is a PRODUCT and date is not provided 
  { 
    if (local) 
    {
      tempString <- strsplit(opts$arcStructure,"/")[[1]]
      
      string <- list()
      l=0
      for (i in 1:length(tempString))
      {
        s <- strsplit(tempString[i],"\\.")[[1]]
    
        if (length(s)>0) 
        {
          tmp <- list()
          for (u in 1:length(s))
          {
            if (s[u] %in% c("DATE","YYYY","DDD")) 
            {
              if (product$PRODUCT!="SRTM")
              {
                tmp[[u]] <- s[u]
              }
            } else 
            {
              tmp[[u]] <- getPart(x=product,s[u])
            }
          }
          if (length(tmp)>0)
          {
            l=l+1
            string[[l]] <- paste0(unlist(tmp),collapse=".")
          }
        }
      }
    localPath <- setPath(path.expand(paste0(opts$localArcPath,paste0(unlist(string),collapse="/"))))
    }
        
    if (remote) 
    {
      namesFTP <- names(MODIS_FTPinfo)
      Hmany <- grep(namesFTP,pattern="^ftpstring*.")
      
      remotePath <- list()
      n = 0
      for (e in Hmany)
      {
        stringX <- MODIS_FTPinfo[[e]]
        
        if(length(grep(product$SOURCE,pattern=stringX$name))>0 & what %in% stringX$content)
        {
          n=n+1                    
          if(is.null(stringX$variablepath))
          {
            remotePath[[n]] <- stringX$basepath
          } else 
          {
            struc      <- stringX$variablepath    
            tempString <- strsplit(struc,"/")[[1]]
    
            string <- list()
            l=0
            for (i in 1:length(tempString))
            {
              s <- strsplit(tempString[i],"\\.")[[1]]
      
              if (length(s)> 0) 
              {
                l=l+1    
                tmp <- list()
                for (u in 1:length(s))
                {
                  if (s[u] %in% c("DATE","YYYY","DDD")) 
                  {
                    if (product$PRODUCT!="SRTM")
                    {
                      tmp[[u]] <- s[u]
                    }
                  } else 
                  {
                    tmp[[u]] <- getPart(x=product,s[u])
                  }
                }                                
                string[[l]] <- paste0(unlist(tmp),collapse=".")    
              }
            }
          remotePath[[n]] <- path.expand(paste(stringX$basepath,paste0(unlist(string),collapse="/"),sep="/"))
          }
          names(remotePath)[n] <- stringX$name
        }
      }
    }
  } else 
  { # if x is a file name
          
    if (local) 
    {
      tempString <- strsplit(opts$arcStructure,"/")[[1]]
  
      string <- list()
      l=0
      for (i in 1:length(tempString))
      {
        s <- strsplit(tempString[i],"\\.")[[1]]
        
        if (length(s)>0)
        {
          l=l+1
          tmp <- list()
          for (u in seq_along(s))
          {
            tmp[[u]] <- getPart(x=product,s[u])
          }
        string[[l]] <- paste0(unlist(tmp),collapse=".")
        }
      } 
    localPath <- setPath(path.expand(paste0(opts$localArcPath,paste0(unlist(string),collapse="/"))))
    }

    if (remote) 
    {
      if (!what %in% c("images","metadata")) 
      {
        stop("Parameter 'what' must be 'images' or 'metadata'")
      }
               
      namesFTP <- names(MODIS_FTPinfo)
      Hmany <- grep(namesFTP,pattern="^ftpstring*.") # get ftpstrings in ./MODIS_opts.R
  
      remotePath <- list()
      n = 0
      for (e in Hmany)
      {
        stringX <- MODIS_FTPinfo[[e]]
        
        if(length(grep(product$SOURCE,pattern=stringX$name))>0 & what %in% stringX$content)
        {
          struc <- stringX$variablepath    
          tempString <- strsplit(struc,"/")[[1]]
      
          string <- list()
          l=0        
          for (i in 1:length(tempString))
          {
            s <- strsplit(tempString[i],"\\.")[[1]]
    
            if (length(s)>0)
            {
              l=l+1
              tmp <- list()
              for (u in seq_along(s))
              {
                tmp[[u]] <- getPart(x=product,s[u])
              }
              string[[l]] <- paste0(unlist(tmp),collapse=".")
            }
          }
          n=n+1
          remotePath[[n]]      <- path.expand(paste(stringX$basepath,paste0(unlist(string),collapse="/"),sep="/"))
          names(remotePath)[n] <- stringX$name
        }
      }        
    }
  }        
  return(list(localPath=correctPath(localPath), remotePath=remotePath))
}

