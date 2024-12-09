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
      grepInParent <- dir(pkgParent, pattern=subdir, 
                         full.names = TRUE, ...)
      subdirDir <- c(subdirDir, grepInParent)
    }
  }
  if(length(subdirDir)>0)return(unique(subdirDir))
##
## 4. Look for subdir in pkg 
##
  for(i in seq(1, length=nFound)){
    for(j in foundLvl[[i]]){
      pkgDir <- paste(foundInWD[[i]][1:j], 
                  collapse=.Platform$file.sep)
      grepInPkg <- dir(pkgDir, pattern=subdir, 
                       full.names = TRUE, ...)
      subdirDir <- c(subdirDir, grepInPkg)
    }
  }
  if(length(subdirDir)>0)return(unique(subdirDir))
##
## 5. Look for subdir in subdir1 in pkgDir
##
  subdir1Found <- FALSE 
  for(i in seq(1, length=nFound)){
    for(j in foundLvl[[i]]){
      pkgDir <- paste(foundInWD[[i]][1:j], 
                collapse=.Platform$file.sep)
      subd1InPkg <- dir(pkgDir, pattern=subdir1, 
                       full.names = TRUE, ...)
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