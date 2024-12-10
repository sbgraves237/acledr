findSubdir <- function(pkg="^acledr$", wd=getwd(), 
        subdir='^extdata$', subdir1='^inst$', ...){
##
## 1. Find the "pkg" package directory
##
  if(wd=="~"){
    cat("Do you really want to search the entire hard drive?", 
        "\n(<ctrl-c> if no.)")
  }
  dirWd. <- dir(wd, recursive = TRUE, 
                full.names=TRUE, ...)
  dirWd <- strsplit(dirWd., 
            .Platform$file.sep)
  nDirr <- length(dirWd)
#
  grepDir <- function(path='.', pattern=NULL, ...){
    short <- dir(path, pattern=NULL, ...)
    long <- dir(path, pattern=NULL, full.names = TRUE, ...)
    names(long) <- short
    g <- grep(pattern, short)
    long[g]
  }
#  
#  whichGrep <- function(pattern, x){
#    Pkg <- which(pattern==x)
#    if(length(Pkg)<1){
#      Pkg <- grep(pattern, x)
#    }
#    Pkg
#  }
#
  whichLvl <- lapply(dirWd, function(x){
                grep(pkg, x)
  })
  maxLvl <- sapply(whichLvl, function(x){
      if(length(x)>0)max(x) else 0 
  })
##  
## 2. discard paths in which pkg not found 
##  
  foundInWD <- dirWd[maxLvl>0]
  foundLvl <- whichLvl[maxLvl>0]
  foundMaxLvl <- maxLvl[maxLvl>0]
  nFound <- length(foundInWD)
  if(nFound<1){
    stop('pkg = ', pkg, ' not found in wd = ', 
         wd)
  }
## 
## 3. Find parents and search in them for subdir
##  
  subdirDir <- character(0)
  for(i in seq(1, length=nFound)){
    for(j in foundLvl[[i]]){
      pkgParent <- paste(foundInWD[[i]][1:(j-1)], 
                collapse=.Platform$file.sep)
      grepInParent <- grepDir(pkgParent, pattern=subdir, 
                              ...)
      subdirDir <- c(subdirDir, grepInParent)
    }
  }
  if(length(subdirDir)>0)return(
        unique(as.character(subdirDir)))
##
## 4. Look for subdir in pkg 
##
  for(i in seq(1, length=nFound)){
    for(j in foundLvl[[i]]){
      pkgDir <- paste(foundInWD[[i]][1:j], 
                  collapse=.Platform$file.sep)
      grepInPkg <- grepDir(pkgDir, pattern=subdir, 
                            ...)
      subdirDir <- c(subdirDir, grepInPkg)
    }
  }
  if(length(subdirDir)>0)return(
          unique(as.character(subdirDir)))
##
## 5. Look for subdir in subdir1 in pkgDir
##
  subdir1Found <- FALSE 
  for(i in seq(1, length=nFound)){
    for(j in foundLvl[[i]]){
      pkgDir <- paste(foundInWD[[i]][1:j], 
                collapse=.Platform$file.sep)
      pkgDir. <- dir(pkgDir)
      pkgDir.. <- dir(pkgDir, full.names = TRUE, 
                      ...)
      
      subd1InPkg. <- grep(subdir1, pkgDir.) 
      subd1InPkg <- pkgDir..[subd1InPkg.] 

      if(length(subd1InPkg)>0){
        subdir1Found <- TRUE
        for(sbd1 in subd1InPkg){
          grepIn1 <- dir(sbd1, pattern=subdir, 
                       full.names=TRUE,...)
          subdirDir <- c(subdirDir, grepIn1)
        }
      }
    }
  }
  if(length(subdirDir)<1){
    if(subdir1Found){
      stop('subdir1 = ', subdir1, 'found but not ', 
           'subdir = ', subdir)
    } else {
      stop('Neither subdir = ', subdir, 'nor subdir1 =', 
           subdir1,' found.')
    }
  }
  unique(subdirDir)
}