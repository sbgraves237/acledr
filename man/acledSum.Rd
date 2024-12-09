\name{acledSum}
\alias{ctryMoEv}
\alias{ctryMo}
\alias{ctryEv}
\alias{ctry}
\alias{MoEv}
\alias{Mo}
\alias{Ev}
\docType{data}
\title{
  Summaries of ACLED data 
}
\description{
  Seven summaries giving counts of events and deaths extracted from the 
  \href{https://en.wikipedia.org/wiki/Armed_Conflict_Location_and_Event_Data}{Armed Conflict Location and Event Data} 
  (ACLED) project, with names matching their binary code: 

\tabular{cl}{
  111 \tab \code{ctryMoEv} = counts by country, month, and event type for 2 months only (2023-12 and 2024-01); much more can violate CRAN's size limits\cr
  110 \tab \code{ctryMo} = counts by country and month between 2021-01 and 2024-09.\cr
  101 \tab \code{ctryEv} = counts by country and event type between 2021-01 and 2024-09.\cr
  100 \tab \code{ctry} = counts by country between 2021-01 and 2024-09.\cr
  011 \tab \code{MoEv} = counts by month and event type between 2021-01 and 2024-09.\cr
  010 \tab \code{Mo} = counts by month between 2021-01 and 2024-09.\cr
  001 \tab \code{Ev} = counts by event type between 2021-01 and 2024-09.\cr
}
}
\usage{data(ctryMoEv)}
\usage{data(ctryMo)}
\usage{data(ctryEv)}
\usage{data(ctry)}
\usage{data(MoEv)}
\usage{data(Mo)}
\usage{data(Ev)}
\format{
  A \code{\link{data.frame}}s with differing numbers of 
  observations and columns that include all of the following 
  (for \code{ctryMoEv}) or some subset (for all others): 
  \describe{
    \item{Country}{
      a character vector giving the name of the country 
      or subnational region used by \code{ACLED}. 
    }
    \item{Event.month}{
      a vector of class "Date" giving the first day of 
      each month for monthly counts. 
    }
    \item{Event.Type}{
      a character vector of either \code{Battles}, 
      \code{Explosions/Remote violence}, 
      \code{Protests}, \code{Riots}, 
      \code{Strategic developments}, or 
      \code{Violence against civilians}. 
    }
    \item{Events, Deaths}{
      integer vectors giving the desired counts
    }
    \item{deathsPerEvent}{
      number giving \code{Deaths} per \code{Event}. 
    }
  }
}
\details{
  Created as discussed in the 
  \code{readACLEDsummaryData} vignette in this 
  \code{acledr} package. 
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
