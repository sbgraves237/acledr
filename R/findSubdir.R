findSubdir <- function(pkg="^acledr$", wd=getwd(), 
                       subdir='^extdata$', 
                       subdir1='^inst$'){
##
## 1. Find the "pkg" package directory
##
  wdList <- strsplit(wd, .Platform$file.sep)[[1]]
  whichGrep <- function(pattern, x){
    Pkg <- which(pattern==x)
    if(length(Pkg)<1){
      Pkg <- grep(pattern, x)
    }
    Pkg
  }
  Pkg <- whichGrep(pkg, wdList)
  if(length(Pkg)<1){
#  pkg not found embedded in wd. Might it be a subdirectory?       
      pkgDir <- dir(wd, pattern=pkg, 
            all.files=TRUE, full.names=TRUE)
      if(length(pkgDir)<1)pkgDir <- wd 
      pkgParent <- wd 
  } else {
      pkgDir <-  do.call(file.path, 
                    as.list(wdList[1:Pkg[1]]))
      if(Pkg[1]>1){
        pkgParent<- do.call(file.path, 
                as.list(wdList[1:(Pkg[1]-1)]))
      } else pkgParent <- wd
  }
##
## 2. Look for subdir in pkgParent 
##
  whichDir <- function(path, pattern){
    Dir <- dir(path, all.files = TRUE)
    Subdp <- whichGrep(pattern, Dir)
    if(length(Subdp)>0){
      return(dir(path, all.files = TRUE, 
                 full.names = TRUE)[Subdp])
    }
    character(0)
  }
  subDir <- whichDir(pkgParent, subdir)
  if(length(subDir)>0)return(subDir)
##
## 3. Look for subdir directly in pkgDir
##
  subDir <- whichDir(pkgDir, subdir)
  if(length(subDir)>0)return(subDir)
##
## 4. Look for subdir1 in pkgDir
##  
  subDi <- whichDir(pkgDir, subdir1)
  if(length(subDi)<1){
    ermsg2 <- paste0('Subdirectory ', subdir, 
      ' not found in either the parent of package ', 
      pkg, ' nor directly in ', pkg, ' itself.\n', 
      'In addition, subdir1 (', subdir1, 
      ') not found.\n')
    stop(ermsg2)
  }
##
## 5. look for subdir in subDi
##
  subDir <- whichDir(subDi, subdir)
  if(length(subDir)>0){
    return(subDir)
  } else {
    ermsg3 <- paste0('Subdirectory ', subdir, 
      ' not found associated with package ', pkg, 
      ', though subdir1 (', subdir1, ') was [', 
      subdi, ']\n')
    stop(ermsg3)
  }
##
## 5. Control should never get here.
##    It should already have returned something 
##    or an error message.   
##
  stop('Logic error in function findSubdir')
}