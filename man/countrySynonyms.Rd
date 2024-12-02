\name{countrySynonyms}
\alias{countrySynonyms}
\docType{data}
\title{
Synonyms of country names for each ISO 3 letter country code to enable conversion
}
\description{
Local copy of \code{\link[rworldmap]{countrySynonyms}}. 

On 2024-12-02 I was not able to get R CMD check to find
\code{countrySynonyms}, even though I had it in 
\code{NAMESPACE}. 
}
\usage{data("countrySynonyms")}
\format{
  A data frame with 282 observations on the following 10 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{ISO3}}{
        ISO-3166 three-letter code to return
        when a match is found.}
    \item{\code{name1}}{most common country name}
    \item{\code{name2}, \code{name3}, \code{name4}, \code{name5}, \code{name6}, \code{name7}, \code{name8}}{
        alternative country names
    }
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(countrySynonyms)
str(countrySynonyms) 
}
\keyword{datasets}
