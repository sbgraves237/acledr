\name{ACLEDpopGDP}
\alias{ACLEDpopGDP}
\docType{data}
\title{
  Population and GDPpc for ACLED countries
}
\description{
  Data on population and average annual income 
  (Gross Domestic Product, GDP, per capita, 
  \code{GDPpc}), both 
  \href{https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)}{nominal}, 
  i.e., at current exchange rates (GDPpcn), and 
  \href{https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)}{at purchasing power parity} 
  (PPP, GDPpcp) for 2020:2023 and average for 
  "countries" in the \code{\link{ctry}} 
  \href{https://en.wikipedia.org/wiki/Armed_Conflict_Location_and_Event_Data}{Armed Conflict Location and Event Data} 
  (ACLED) data. 
  
  
         ACLEDpopGDP <- data.frame(ACLEDcountry=names(ACLEDctryCode), 
  ISO3=as.character(ACLEDctryCode), WBcountry=NA, 
  pop2020=NA, pop2021=NA, pop2022=NA, pop2023=NA, pop=NA, 
  GDPpcn2020=NA, GDPpcn2021=NA, GDPpcn2022=NA, GDPpcn2023=NA, 
  GDPpcn=NA, 
  GDPpcp2020=NA, GDPpcp2021=NA, GDPpcp2022=NA, GDPpcp2023=NA, 
  GDPpcp=NA, Comments=NA)
rownames(ACLEDpopGDP) <- names(ACLEDctryCode)

}
\usage{
  data(ACLEDpopGDP)
}
\format{
  A \code{\link{data.frame}} with \code{\link{rownames}}
  being the "country" (and subnational region) names 
  used by ACED and the following columns: 
  \describe{
    \item{ACLEDcountry, ISO3}{
      Character vectors giving the names of the countries 
      or subnational region used by ACLED and the 
      corresponding 3-letter 
      \href{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}{ISO 3166 country codes}. 
    }
    \item{WBcountry}{
      Name of the country used by the World Bank.
    }
    \item{pop2020, pop2021, pop2022, pop2023, pop}{
      population estimates for 2020:2023 and 
      average for those years. For countries in 
      \code{\link{WBdat}}, these are World Bank 
      numbers. For ACLED countries not in the 
      standard World Bank data extraction tool 
      used to create \code{WBdat}, these numbers 
      come from other sources. 
    }
    \item{GDPpcn2020, GDPpcn2021, GDPpcn2022, GDPpcn2023, GDPpcn}{
      Nominal GDPpc numbers, i.e., at current 
      exchange rates for 2021:2023 and average. 
      As with population numbers, these come from 
      the World Bank where available and from other
      sources for \code{ACLEDcountry} not in the 
      standard World Bank data extraction tool 
      used. 
    } 
    \item{GDPpcp2020, GDPpcp2021, GDPpcp2022, GDPpcp2023, GDPpcp}{
      GDPpc numbers at 
      \href{https://en.wikipedia.org/wiki/Purchasing_power_parity}{Purchasing Power Parity} 
      (PPP) for 2021:2023 and average. As with 
      population and \code{GDPpcn}numbers, these 
      come from the World Bank where available and 
      from other sources for \code{ACLEDcountry} 
      not in the standard World Bank data extraction 
      tool used. 
    } 
    \item{Comments}{
      A character vector that is blank for 
      countries in the World Bank data extraction
      tool used and describing the source for other 
      "countries" (subnational entities). 
    }
  }
}
\details{
  \code{GDPpcn} numbers are in constant 2015 US 
  dollars. \code{GDPpcp} are in constant 2021 
  international dollars. Total population are 
  midyear estimates. 
}
%\source{}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(ACLEDpopGDP)
str(ACLEDpopGDP)
}
\keyword{datasets}
