\name{acledr-package}
\alias{acledr-package}
\alias{acledr}
\docType{package}
\title{
  Merge and analyze Armed Conflict Location and Effect Data 
  (\code{ACLED})
}
\description{
  Merge and analyze data from Armed Conflict Location and Event 
  Data (\code{ACLED}) and possibly other sources, e.g., World 
  Bank.  
}
\section{Introduction}{
  With luck, \pkg{acledr} will eventually provide vignettes 
  describing how to do the following: 
  
  \enumerate{
    \item {
      \code{acledr_readACLEDsummarydata}
      Access \code{ACLED} summary data, read it into R as 
      a \code{\link{data.frame}}, then save it with a few 
      summaries as \code{data.frame}s in the package. 
    }
    \item {
      Create \code{ACLEDctryCode} named character vector
      to facilitate translation between the "country" or 
      geographic entity included in the \code{ACLED} 
      summary data and the corresponding 3-letter 
      \href{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}{ISO 3166 country code}.
    }
    \item {
      Read and save a World Bank data extract and save
      as \code{WBdat}. 
    }
    \item {
      Merge various \code{ACLED} data sets with 
      \code{WBdat} and save for subsequent analysis. 
    }
    \item {
      Analyze the merger of \code{ACLED} and 
      \code{WB} data. 
    }
  }
  
  \code{acledr} version 0.0-1 only contains a 
  framework to which the vignettes just enumerated 
  will be added. Subsequent versions will likely 
  contain more of what is being planned here. 
}
%\section{Usage for package building}{
%  The \dQuote{Rcpp-package} vignette documents how to use Rcpp in client packages.
%}
\author{Spencer Graves}
%\references{
%}
%\section{History}{
%}
%\seealso{
%}
%\examples{
%}
\keyword{database}
\keyword{models}
\keyword{ts}