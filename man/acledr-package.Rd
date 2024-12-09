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
      \code{readACLEDsummarydata}: 
      Access \code{ACLED} summary data, read it into R as 
      a \code{\link{data.frame}}, then save it with a few 
      summaries as \code{data.frame}s in the package 
      described in the help file \code{link{acledSum}}.
    }
    \item {
      \code{readWorldBankData}: Access data downlaoded 
      from the World Bank for analysis with \code{ACLED}
      data, read and save it as a dataset called 
      \code{WBdat} associated with this \code{acledr}
      package. 
    }
    \item {
      Create \code{ACLEDctryCode} named character vector
      to facilitate translation between the "country" or 
      geographic entity included in the \code{ACLED} 
      summary data and the corresponding 3-letter 
      \href{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}{ISO 3166 country code}, 
      which can further be used to merge ACLED data with 
      data from other sources, e.g., World Bank, World 
      Health Organization, etc. 
    }
    \item {
      Merge various \code{ACLED} data sets with 
      \code{WBdat}, do simple analyses and save for 
      subsequent analysis. 
    }
  }
  
  \code{acledr} version 0.0-2 includes the first 
  of the 5 R Markdown vignettes just outlined and 
  the datasets created by it. Future versions 
  will contain more of these vignettes. 
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