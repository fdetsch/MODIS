#' Reorganize MODIS Files in Local Data Archive
#' 
#' @description 
#' Reorganize the storage structure of your MODIS archive according to the 
#' settings in `options("MODIS_arcStructure")`. Depending on the specified 
#' 'source', you can also use this function to gather all MODIS grid files on 
#' your machine and reorganize them. The main purpose is to organize the 
#' archive, but it is also possible to copy a subset of files to a desired 
#' location!
#' 
#' @param from `character`. Local path to look for MODIS files, defaults to 
#'   `options("MODIS_localArcPath")` (see [MODISoptions()]).
#' @param to `character`. Target folder to move (or copy) MODIS files to, 
#'   defaults to `options("MODIS_localArcPath")`.
#' @param structure `character`. Storage structure, defaults to 
#'   `options("MODIS_arcStructure")` (see Examples).
#' @param pattern Regular expression passed to [list.files()]. Insert a pattern 
#'   if you want to extract specific files from your archive.
#' @param move `logical`. If `TRUE`, files are moved and duplicated files are 
#'   deleted. If `FALSE` (default), files are just copied and thus remain in the
#'   origin folder. Note that the copying process performs rather slowly when 
#'   dealing with large files, e.g. 250-m `"MOD13Q1"`.
#' @param quiet `logical`, defaults to `FALSE`.
#' 
#' @return 
#' If `quiet = FALSE` (default), information on how many files have been moved 
#' (or copied) and deleted is printed to the console.
#' 
#' @author 
#' Matteo Mattiuzzi
#' 
#' @examples 
#' \dontrun{
#' # MOVE all MODIS grid data to the directory and structure as defined in 
#' # options("MODIS_localArcPath", "MODIS_arcStructure")
#' orgStruc(move = TRUE)
#' 
#' # COPY all MOD13Q1 from 2001 to folder "MyFiles/MOD13Q1.collection/"
#' orgStruc(pattern="MOD13Q1.A2001*.",to="MyFiles",structure="PRODUCT.CCC")
#' 
#' # COPY all MOD13Q1 to folder "MyFiles/"
#' orgStruc(pattern="MOD13Q1.*.",to="MyFiles",structure="") 
#' }
#' 
#' @export orgStruc
#' @name orgStruc
orgStruc <- function(from,to,structure, pattern, move=FALSE, quiet=FALSE)
{	
	
	opts <- combineOptions()
  if (missing(from))
  {
      from <- opts$localArcPath
  }
  
  if (missing(to))
  {
      to <- opts$localArcPath
  }
  to <- setPath(to)
  
  if (!missing(structure))
  {
    opts$arcStructure <- structure
  }
    ###########################
    
  if(missing(pattern)) 
  {
  	cat(paste0("No 'pattern' set, moving/coping all MODIS grid data found in '", from,"'.\n"))
  	avFiles <- list.files(from, pattern=".hdf$", recursive=TRUE, full.names=TRUE)
  } else 
  {
  	avFiles <- list.files(from, pattern=pattern, recursive=TRUE, full.names=TRUE)
  }
  
  if (length(avFiles)==0) {stop("No HDF nor HDF.XML files found!\n")}
  doit <- isSupported(avFiles)
  if (sum(doit)==0) {stop("No supported files Found")}
  avFiles <- avFiles[doit]
  
  if (!quiet)
  {
  	cat("Found",length(avFiles),"files \n")
  }
  #########################
  moved <- sapply(avFiles,function(x) 
  {

  	orpath  <- correctPath(dirname(x))
    fname   <- basename(x)
    ########################
    # generate and create local path to file!
    path <- genString(x=fname,remote=FALSE,localArcPath=to)$localPath
    dir.create(path,showWarnings=FALSE,recursive=TRUE)
    ###################
  
    if (!file.exists(file.path(path,fname,fsep="/"))) 
    { # if file doesn't exist in destdir copy/move

	    if (move) 
	    {
		    file.rename(from=x,to=paste0(path,fname))			
		    if (file.exists(paste0(x,".xml"))) 
		    {
			    file.rename(from=paste0(x,".xml"),to=paste0(path,fname,".xml",sep=""))	
		    }
		    moved <- 1
	    } else 
	    {
		    file.copy(from=x,to=paste0(path,fname),overwrite=FALSE)
		    if (file.exists(paste0(x,".xml"))) 
		    {
			    file.copy(from=paste0(x,".xml"),to=paste0(path,fname,".xml"))	
		    }
		    moved <- 2
	    }

    } else if (file.exists(file.path(path,fname,fsep="/")) & orpath!=path & move) 
    { # if file exists in destdir & inpath!=outPath...it is duplicated in 2 different locations, so remove it
        unlink(x)
	    if (file.exists(paste0(x,".xml"))) 
	    {
		    unlink(paste0(x,".xml"))	
	    }
	    moved <- 3
    } else 
    {
	    moved <- 0
    }
    if (length(list.files(orpath))==0) 
    {
	    if (.Platform$OS=="unix") 
	    { # I'm looking for a windows/MAC(?) eqal to the linux "rmdir -p" command!!
		    warn <- getOption("warn") 
		    options(warn=-2)
		    try(xxx <- invisible(system(paste0("rmdir -p --ignore-fail-on-non-empty ", orpath),intern=TRUE)),silent=TRUE)
		    options(warn=warn)
	    } else 
	    { # work around for rmdir -p on windows/MAC(?)
		    unlink(orpath,recursive=TRUE)
		    secPath <- strsplit(orpath,"/")[[1]]
		
		    for (o in length(secPath):1)
		    {
			    delpath <- paste0(secPath[-o:-length(secPath)],collapse="/")

			    if (length(list.files(delpath))==0)
			    {
				    unlink(delpath,recursive=TRUE)
			    } else
			    {
			      break
			    }
		    }
	    } 
    }
    return(moved)
  })

  if (sum(moved==0)==length(avFiles)) 
  {
	  cat("All files in the query are fine, no files to move or to copy!\n") 
  } else 
  {
	  cat("Moved files", sum(moved==1),"\n")
	  cat("Copied files", sum(moved==2),"\n")
	  cat("Not moved files", sum(moved==0),"\n")
	  cat("Deleted multiple files", sum(moved==3),"\n")
  }
}

