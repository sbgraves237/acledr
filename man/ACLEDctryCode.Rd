\name{ACLEDctryCode}
\alias{ACLEDctryCode}
\title{
  ACLED country codes
}
\description{
  Three-letter ISO-3166 codes for all the "Country" codes 
  in the {ACLED} database. 
}
\usage{
  data(ACLEDctryCode)
}
\format{
  A character vector of three-letter country codes for all 
  countries in \code{\link{ctry}} with the names used by 
  ACLED. 
}
\details{
  Created as discussed in the \code{ACLEDctryCode} 
  vignette in this \code{acledr} package. 
}
\source{
  \href{https://en.wikipedia.org/wiki/Armed_Conflict_Location_and_Event_Data}{Armed Conflict Location and Event Data} 
  (ACLED) project extracted using their 
  \href{https://acleddata.com/explorer/}{ACLED Explorer}
  as described in the 
  \code{readACLEDsummaryData} vignette in this 
  \code{acledr} package. 
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(ctryMoEv)
data(ctry)
str(ctryMoEv)
str(ctry) 
}
\keyword{datasets}
