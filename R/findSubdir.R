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
  nPkg <- length(Pkg)
  if(nPkg<1){
#  pkg not found embedded in wd. Might it be a subdirectory?       
    pkgDir <- dir(wd, pattern=pkg, 
            all.files=TRUE, full.names=TRUE)
    if(length(pkgDir)<1)pkgDir <- wd 
    pkgParent <- wd 
  } else {
    pkgDir <- character(nPkg)
    for(i in 1:nPkg){
      pkgDir[i] <-  do.call(file.path, 
                    as.list(wdList[1:Pkg[i]]))
    }
    if(Pkg[1]>1){
        pkgParent<- do.call(file.path, 
                as.list(wdList[1:(Pkg[1]-1)]))
    } else pkgParent <- wd
  }
##
## 2. Look for subdir in pkgParent 
##
  whichDir <- function(path, pattern, ...){
    cat('In whichDir, class(path) =\n', 
      paste(class(path), collapse=', '), '\n')
    cat('In whichDir, path =\n', path, 
        '\npattern = ', pattern,'\n')
    Dir <- dir(path, all.files = TRUE, ...)
    Subdp <- whichGrep(pattern, Dir)
    if(length(Subdp)>0){
      return(dir(path, all.files = TRUE, 
                 full.names = TRUE)[Subdp])
    }
    cat('whichDir(path=', path, ', pattern=', pattern, 
        ') found nothing.\n')
    character(0)
  }
  subDir <- whichDir(pkgParent, subdir)
  if(length(subDir)>0)return(subDir)
##
## 3. Look for subdir directly in pkgDir
##
  subDir <- vector('list', nPkg)
  for(i in seq(1, length=nPkg)){
    subDir[[i]] <- whichDir(pkgDir[i], subdir)
    if(length(subDir[[i]])>0)return(subDir[[i]])
  }
##
## 4. Look for subdir1 in pkgDir
## 
  subDi <- vector('list', nPkg)
  nsubDi <- integer(nPkg)
  for(i in seq(1, length=nPkg)){
    subDi[[i]] <- whichDir(pkgDir[i], subdir1)
    if((length(subDi[[i]])>0) && (nchar(subDi[[i]])>0)){
      nsubDi[i] <- nchar(subDi[[i]])
    }
  }
  if(sum(nsubDi)<1){
    ermsg2 <- paste0('Subdirectory ', subdir, 
      ' not found in either the parent of package ', 
      pkg, ' nor directly in ', pkg, ' itself.\n', 
      'In addition, subdir1 (', subdir1, 
      ') not found.\n')
    cat('in findSubdir with pkg = ', pkg, ',\n')
    cat('  wd = ', wd, ',\n')
    print(wdList)
    cat('  subdir = ', subdir, ',\n')
    cat('  and subdir1 = ', subdir1, ',\n')
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
      subDi, ']\n')
    stop(ermsg3)
  }
##
## 5. Control should never get here.
##    It should already have returned something 
##    or an error message.   
##
  stop('Logic error in function findSubdir')
}