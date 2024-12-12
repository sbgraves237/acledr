\name{grepInTable}
\alias{grepInTable}
\title{
  grep for each element of a character vector pattern in referenceTable
}
\description{
  \code{\link{grep}} for each element of a character 
  vector \code{pattern} in the columns of 
  \code{referenceTable[, -1]}, return elements of 
  \code{referenceTable[, 1]} corresponding to rows
  identified in the first column of 
  \code{referenceTable[, -1]} in which anything was
  found. 
  
  The default \code{referenceTable} is 
  \code{countrySynonyms}, a local copy of 
  \code{\link[rworldmap]{countrySynonyms}}[, -1], 
  which returns 3-letter country codes per 
  \href{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}{ISO-3166}. 
  
  Return a named character vector of the contents 
  of \code{referenceTable[, 1]} found, with names 
  being \code{pattern}. If no match is found, 
  return an appropriate error message for that 
  element of \code{pattern}. If an empty string, 
  \code{''}, is found, complain about that. 
}
\usage{
grepInTable(pattern, referenceTable = 
      countrySynonyms[, -1], ignore.case=TRUE, 
      collapse=', ', ...)
}
\arguments{
  \item{pattern}{ 
    Character vector of matches of pattern[i] in 
    different columns of 
    \code{referenceTable[, -1]} until a match is found.  
  }
  \item{referenceTable}{
    \code{\link{data.frame}} with the first column
    containing reference codes to be returned if found. 
    
    if(\code{ignore.case}) force \code{pattern} and 
    \code{referenceTable} \code{\link{toupper}} 
    before searching for matches. 
  }
  \item{ignore.case}{
    If \code{TRUE}, force all \code{\link{toupper}}. 
  }
  \item{collapse}{
    Ignored if only one match is found. Otherwise, 
    return 
    \code{paste0(referenceTable[ix, 1], collapse=collapse)} 
    where \code{ix} = the indices of all matches for 
    \code{pattern[i]}. 
  }
  \item{\dots}{
    optional arguments for \code{\link{grep}}. 
  }
}
\details{
  1. if(\code{ignore.case}) Force \code{pattern} and 
  all columns of 
  \code{referenceTable} \code{\link{toupper}}. 
  
  2. \code{\link{grep}} for \code{string[i]} in 
  \code{referenceTable[-1} and return the 
  corresponding element of 
  \code{referenceTable[1]} if found and an 
  appropriate message otherwise. 
}
\value{
  A character vector of the contents of 
  \code{referenceTable[1]} for all matches
  or not-found messages that include 
  \code{string[i]} when \code{string[i]}
  is not found. 
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{grep}}, \code{\link{toupper}}, 
  \code{\link[rworldmap]{countrySynonyms}}
}
%\references{}
\examples{
# Test with three countries in the default referenceTable
tstCodes <- grepInTable(
    c('Iran', 'Christmas Island', 'eSwatini'))
    
answer <- c(Iran='IRN', 'Christmas Island' =
  paste("Found in row 53 of referenceTable with column 1 = ''"),  
  'eSwatini'="No match found for eSwatini")    

\dontshow{
stopifnot(
}
all.equal(tstCodes, answer)
\dontshow{)}

# Test using WBdat as referenceTable
tstCd2 <- grepInTable(
    c('Iran', 'Christmas Island', 'eSwatini'), 
    WBdat[4:3])
    
answ2 <- c(Iran='IRN', 'Christmas Island' =
        "No match found for Christmas Island",
  'eSwatini'="SWZ")    

\dontshow{
stopifnot(
}
all.equal(tstCd2, answ2)
\dontshow{)}
}    
\keyword{manip}
