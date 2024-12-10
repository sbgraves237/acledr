\name{WBdat}
\alias{WBdat}
\docType{data}
\title{
  World Bank data on population and GDPpc for ACLED
}
\description{
  World Bank data on population and average annual 
  income (Gross Domestic Product, GDP, per cap, 
  `GDPpc`, both nominal and at purchasing power 
  parity (PPP) for 2021-2023. 
}
\usage{
  data(WBdat)
}
\format{
  A \code{\link{data.frame}} with the following 
  columns: 
  \describe{
    \item{Series, SeriesCode}{
      Character vectors giving the name of the three
      variables and the corresponding coded name:
      \tabular{rr}{
        \bold{Series} \tab \bold{SeriesCode}\cr
        GDP per capita (constant 2015 US$) \tab NY.GDP.PCAP.KD\cr
        GDP per capita, PPP (constant 2021 international $) \tab NY.GDP.PCAP.PP.KD\cr
        Population, total \tab SP.POP.TOTL\cr
      }
    }
    \item{Country, CtryCode}{
      Character vectors giving the names of the countries 
      or subnational region used by the World Bank and the 
      corresponding 3-letter 
      \href{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}{ISO 3166 country codes}. 
    }
    \item{YR2020, YR2021, YR2022, YR2023, avg}{
      numeric values of the variables for 2020:2023 and 
      their \code{\link{rowMeans}} (with \code{na.rm} = 
      TRUE). 
    }
  }
}
\details{
  A 2024-10-28 email from the ACLED Access Team recommended 
  matching their data with World Bank data using their 
  \href{https://data.worldbank.org/}{open data portal}. 
  From there, it did not seem obvious how to get the desired
  information by country and year, let alone by month. 
  
  Fortunately, clicking on 
  \href{https://databank.worldbank.org/home.aspx}{DataBank}
  produced something useful. A next step involved clicking on 
  the right under 
  \href{https://databank.worldbank.org/indicator/SP.POP.TOTL/1ff4a498/Popular-Indicators}{"What's popular"}.
  That then displayed numbers by country and year from 2008 to 
  2015. That was refined by clicking "Add Time" then selecting
  2020, 2021, 2022, and 2023. The next step from there was 
  clicking "x" in the upper right and "Apply Changes". 

  A next step from there was clicking on "Add Series". That 
  produced a list of variables, many of which were already 
  selected. Clicking "Unselect all" eliminated many series 
  that did not seem appropriate for this application. That was 
  followed by clicking the three \code{Series} listed above. 
  
  The process was completed by clicking "Download options", then 
  "Excel". That downloaded a file named 
  \code{P_Popular Indicators.xlsx} which was moved to an  
  \code{extdata} directory associated with this package, 
  \code{acledr}, and changing its name by adding the current 
  date in compressed ISO 8601 format as 20231113. 
  
  Opening \code{P_Popular Indicators20231113.xlsx} exposed
  a tab for "Series - Metadata". This included in columns C 
  and D "GDP per capita (constant 2015 US$)", "GDP per capita, 
  PPP (constant 2021 international $)", and "Total population 
  ... are midyear estimates". 
}
\source{
  \href{https://databank.worldbank.org/home.aspx}{DataBank}
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(WBdat)
str(WBdat)
}
\keyword{datasets}
