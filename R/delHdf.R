#' Delete Local MODIS Grid Files
#' 
#' @description 
#' Delete MODIS grid files to reduce the local storage.
#' 
#' @param product `character`, see [getProduct()].
#' @param collection `character` or `integer`, see [getCollection()].
#' @param extent Extent information, defaults to `"global"`. See [getTile()].
#' @param tileH,tileV `numeric` or `character`. Horizontal and vertical tile 
#'   number, see [getTile()].
#' @param begin,end `Date` or `character`. Begin and end date of MODIS time 
#'   series, see [transDate()].
#' @param ask `logical`. If `TRUE` (default), the user is being asked for 
#'   deletion after checking.
#' @param ... Arguments passed to [MODISoptions()], particularly 'localArcPath'.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' \dontrun{
#' # REMOVE "MYD11A2" from specific date range and area subset:
#' # delHdf(product="MYD11A2",begin="2010001",end="2010.02.01",extent="austria")
#' # or
#' # delHdf(product="MYD11A2",begin="2010001",end="2010.02.01",tileV=18:19,tileH=4)
#' 
#' # REMOVE "MOD11A2" and "MYD11A2" from specific date range but globaly:
#' # delHdf(product="M.D11A2",begin="2010001",end="2010.02.01") 
#' 
#' # REMOVE ALL "MOD11A2" from local archive:
#' # delHdf(product="MOD11A2") 
#' }
#' 
#' @export delHdf
#' @name delHdf
delHdf <- function(product, collection=NULL, extent="global", tileV=NULL, tileH=NULL, begin=NULL, end=NULL, ask=TRUE,...)
{
    if (!ask)
    {
        doit <- "Y" 
    }
    
    summaries <- 0
   
    opts <- combineOptions(...)

    # product/dates/extent
    product     <- getProduct(x = product, quiet = TRUE, collection = collection)

    info <- list()
    for (z in seq_along(product@PRODUCT))
    {    
       info[[z]] <-  paste0(product@PRODUCT[z],".",product@CCC[[which(names(product@CCC)==product@PRODUCT[z])]])
    }
    cat("\nYou are about to delete\n - products:", paste(unlist(info),collapse=", "),"\n")
    rm(info)

    if (!is.null(tileV)&!is.null(tileH))
    {
        ext <- getTile(tileV=tileV,tileH=tileH)
    } else if (extent[1]!="global")
    { 
        ext <- getTile(x = extent)
    } else 
    {
        ext      <- list()
        ext$tile <- extent
    }
    
    cat(" - tiles:", paste0(unlist(ext$tile),collapse=", "),"\n")
    
    if (is.null(begin) & is.null(end))
    {
        cat(" - dates: all dates\n")
    } else {
        tLimits <- transDate(begin=begin,end=end)
        cat(" - date range: from",as.character(tLimits$begin),"to",as.character(tLimits$end),"\n")
    }

    if (ask)
    {
        doit <- toupper(readline("\nAre you sure you want proceed? [y/n]: "))
    }
    if (doit %in% c("N","NO"))
    {
        return("Ok deleting abborted!")
        
    } else if (doit %in% c("Y","YES")) 
    {
        # bypass checks if a complete product has to be deleted! 
        if (is.null(begin) & is.null(end) & ext$tile[1]=="global")
        {
            for (z in seq_along(product@PRODUCT))
            {
                todo <- paste0(product@PRODUCT[z],".",product@CCC[[which(names(product@CCC)==product@PRODUCT[z])]])
        
                for(u in seq_along(todo))
                {
                    path      <- genString(x=strsplit(todo[u],"\\.")[[1]][1],collection=strsplit(todo[u],"\\.")[[1]][2],remote=FALSE,opts)$localPath
                    path      <- strsplit(path,"/")[[1]]
                    path      <- paste0(path[-length(path)],collapse="/")
                    allLocal  <- list.files(path,recursive=TRUE)
                    summaries <- fileSize(allLocal,units="MB") + sum(summaries)
                    unlink(path,recursive=TRUE)
                }
            }
            cat("Deleted:", todo[u],"\n")
            
        } else 
        { 
            for (z in seq_along(product@PRODUCT))
            {
                todo <- paste0(product@PRODUCT[z],".",product@CCC[[which(names(product@CCC)==product@PRODUCT[z])]])
            
                for(u in seq_along(todo))
                {
                    path <- genString(x=strsplit(todo[u],"\\.")[[1]][1],collection=strsplit(todo[u],"\\.")[[1]][2],remote=FALSE,opts)$localPath
                    path <- strsplit(path,"/")[[1]]
                    path <- paste0(path[-length(path)],collapse="/")
                    
                    allLocal <- list.files(path=path,pattern=".hdf",recursive=TRUE,full.names=TRUE)
                    if (length(allLocal)!=0)
                    {        
                        # remove out of querry dates
                        locDates <- as.Date(sapply(allLocal,function(x)
                        {
                            date <- strsplit(normalizePath(dirname(x),winslash="/"),"/")[[1]]
                            date <- date[length(date)]
                            return(date)
                        }),"%Y.%m.%d")
                        
                        allLocal <- allLocal[as.Date(tLimits$begin) <= locDates & as.Date(tLimits$end) >= locDates]          
                        
                        if (length(allLocal)==0) 
                        {    
                            break
                        }
                        
                        subprod <- getProduct(allLocal[1])

                        if (subprod@TYPE=="CMG")
                        {
                            useExt <- "global"
                        } else {
                            useExt <- ext$tile
                        }
                       
                        
                        if (useExt=="global")
                        {
                            summaries <- fileSize(allLocal,units="MB") + sum(summaries)
                            unlink(allLocal,recursive=TRUE)
                        } else {
                        
                            for(i in seq_along(useExt))
                            {
                                summaries <- fileSize(allLocal,units="MB") + sum(summaries)
                                unlink(grep(allLocal,pattern=useExt[i],value=TRUE),recursive=TRUE)
                            }
                        }
                        dirs <- unique(dirname(allLocal))
                        for (i in seq_along(dirs))
                        {
                            if (length(list.files(dirs[i]))==0)
                            {
                                if (.Platform$OS=="unix") # I'm looking for a windows/MAC(?) eqal to the linux "rmdir -p" command!!
                                { 
                                    warn <- options()$warn     
                                    options(warn=-2)
                                    try(xxx <- invisible(system(paste0("rmdir -p --ignore-fail-on-non-empty ", dirs[i]),intern=TRUE)),silent=TRUE)
                                    options(warn=warn)
                                } else 
                                { # work arount for rmdir -p on Windows
                                    
                                    unlink(dirs[i],recursive=TRUE)
                                    secPath <- strsplit(dirs[i],"/")[[1]]
                            
                                    for (o in length(secPath):1)
                                    {
                                        delpath <- paste0(secPath[-o:-length(secPath)],collapse="/")
                                        if (length(list.files(delpath))==0)
                                        {
                                            unlink(delpath,recursive=TRUE)
                                        } else {
                                            break
                                        }
                                    }
                                } 
                            }
                        }
                        cat("Deleted subset of:", todo[u],"\n")                
                    } else {
                        cat("No files in querry for:", todo[u],"\n")
                    }                      
                }   
            }
        }
    }   
return(summaries)
}

