\name{findSubdir}
\alias{findSubdir}
\title{
  find subdir in or near a package directory
}
\description{
  Find \code{subdir} as a subdirectory of 
  the the parent director of the \code{pkg} 
  package or in \code{pkg} itself or in its 
  \code{subdir1} subdirectory. 
}
\usage{
findSubdir(pkg='^acledr$', wd=getwd(), 
    subdir='^extdata$', subdir1='^inst$')
}
\arguments{
  \item{pkg}{ 
    A character string or 
    \href{https://stat.ethz.ch/R-manual/R-patched/library/base/html/regex.html}{regular expression} 
    (\code{\link{regex}}) giving the name of 
    the package. (If multiple matches are 
    found, they will be searched in succession 
    until a match is found -- or throw an 
    error if none are found.) 
  }
  \item{subdir}{
    A regular expression for the desired 
    subdirectory to be found first in the parent
    of \code{pkg}. It it is not found there, 
    then a search is made in \code{pkg}. If it is
    not found in either of those, a search is 
    made in \code{subdir1}. 
  }
  \item{wd}{
    A regular expression giving a path either 
    including \code{pkg} or to its immediate
    parent. 
  }
  \item{subdire1}{
    A subdirectory of \code{pkg} in which to 
    search for \code{subdir} if it is not found
    in either the parent of \code{pkg} or in 
    \code{pkg} itself. 
  }
}
\details{
  Section "7.3 Raw data file" of 
  \href{https://r-pkgs.org/data.html#sec-data-extdata}{R Packages} 
  by Wickham and Bryan (accessed 2024-12-06) 
  recommends placing data files used to test 
  read functions in \code{~inst/extdata} in 
  the source directory for the package. However, 
  when the package is compiled and distributed, 
  anything in the \code{inst} directory gets 
  elevated to the parent. 
  
  However, to allow testing a vignette with 
  different files during package development, 
  the \code{findSubdir} function looks first 
  for \code{subdir} of the parent of \code{pkg}, 
  then in \code{pkg}, and finally in 
  \code{subdir1} of the \code{inst} 
  subdirectory of \code{pkg}. 
  
  NOTE: Wickham and Bryan seemed to suggest 
  using either \code{pkgload::shim_system.file} 
  or \code{fs::path_package} for this. I was not 
  able to get either to work. This has the added 
  advantage of supporting testing via 
  \code{GitHub} action using data on a local 
  computer that is diferent from what is 
  available on \code{GitHub}. This allows testing 
  locally with data exceeding 
  \href{https://www.thecoatlessprofessor.com/programming/r/size-and-limitations-of-packages-on-cran/}{CRAN's 5 MB limit while testing on \code{GitHub}}
  is done with data meeting those limits.
  
  NOTE: If the host for the project on 
  \code{GitHub} has another \code{GitHub}
  project named \code{subdir} 
  (\code{"extdata"} by default), this 
  function might find the full path to that 
  other project. This "feature" has not been 
  verified. For present purposes, we assume 
  that is not the case. 
}
\value{
  A character vector giving the full path to 
  the directory or directoris found or an 
  error describing what was not found. 
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{system.file}}, 
  \code{\link[fs]{path_package}}, 
  \code{\link{regex}}
}
\references{
  TheCoatlessProfessor (2016-01-17) "Size 
  and Limitations of Packages on CRAN", 
  \href{https://blog.thecoatlessprofessor.com/programming/r/size-and-limitations-of-packages-on-cran/index.html}{TheCoatless Professor blog}
  
  Hadley Wickham and Jennifer Bryan, 
  \href{https://r-pkgs.org}{R Packages (2e)}
  (online). 
}
\examples{
# Possible multiple matches
  Extdata <- findSubdir('acledr')
  if(nchar(Extdata)<1){
    stop('subdir ', subdir, ' not found in', 
    ' package ', pkg)
  }
# Ensure only one exact match
  Extdata1 <- findSubdir('^acledr$')
  if(nchar(Extdata!)<1){
    stop('subdir ', subdir, ' not found in', 
    ' package ', pkg)
  }
}    
\keyword{data}
\keyword{file}
