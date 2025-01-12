\name{acledr-package}
\alias{acledr-package}
\alias{acledr}
\docType{package}
\title{
  Merge and analyze Armed Conflict Location and Effect Data 
  (\code{ACLED})
}
\description{
  Merge and analyze data from 
  \href{https://en.wikipedia.org/wiki/Armed_Conflict_Location_and_Event_Data}{Armed Conflict Location and Event Data}
  (ACLED) and other sources, e.g., World Bank.  
}
\section{Introduction}{
  With luck, \pkg{acledr} will eventually provide vignettes 
  describing how to do the following: 
  
  \enumerate{
    \item {
      \code{readACLEDsummarydata}: 
      Access ACLED summary data, read it into R as a 
      \code{\link{data.frame}}, then save it with a few 
      summaries as \code{data.frame}s in the formats 
      described in the help file \code{\link{acledSum}}.
    }
    \item {
      \code{readWorldBankData}: Access data downlaoded 
      from the World Bank for analysis with \code{ACLED}
      data, read and save it in the format described in 
      the help file \code{\link(WBdat}}. 
    }
    \item {
      Create \code{ACLEDctryCode}, a named character 
      vector to facilitate translation between the 
      "country" or geographic entity included in the 
      ACLED summary data and the corresponding 3-letter 
      \href{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}{ISO 3166 country code}, 
      which can further be used to merge ACLED data with 
      data from other sources, e.g., World Bank, World 
      Health Organization, etc. 
    }
    \item{ 
       Create \code{ACLEDpopGDP}, a  
       \code{\link{data.frame}} of ACLED country names 
       and ISO 316 three-letter codes plus the name of 
       the country used by the World Bank and population
       and average annual income (Gross Domestic Product 
       per capita, GDPpc), both 
       \href{https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)}{nominal}, 
       i.e., at current exchange rates (GDPpcn), and 
       \href{https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)}{at purchasing power parity} 
       (PPP, GDPpcp) for 2020:2023 and average. 
    }
    \item {
      \code{ACLED_WBmerge}: Merge various \code{ACLED} data 
      sets with \code{ACLEDpopGDP}, do simple analyses, and 
      save for subsequent analyses. 
    }
  }
  
  \code{acledr} version 0.0-3 includes the first 
  three of the five R Markdown vignettes just outlined 
  and the datasets created by them. Future versions 
  will contain the remaining vignettes and possibly 
  more. 
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