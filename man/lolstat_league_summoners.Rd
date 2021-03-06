% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/League.R
\name{lolstat_league_summoners}
\alias{lolstat_league_summoners}
\title{lolstat League}
\usage{
lolstat_league_summoners(division, tier, queue, page = 1, region, APIKey)
}
\arguments{
\item{division}{Character, I to IV}

\item{tier}{Character, IRON to DIAMOND (below MASTER)}

\item{queue}{Character, Game type}

\item{page}{Integer, start index, should be larger than 0}

\item{region}{Character, league region}

\item{APIKey}{Character, if set explicitly; not needed if key is set globally. See \code{\link{setAPIKey}}.}
}
\value{
a data frame consists of summoners name and accountID of Each
}
\description{
Get summoners information in league
}
\details{
for more information, refer official API page, https://developer.riotgames.com/apis#league-v4
}
\examples{
\dontrun{
 # use setAPIKey() to set APIKey
 setAPIKey('myAPIKey', 'sysenv')

 # getAPIKey() to get APIKey
 APIKey = getAPIKey('sysenv')

 # get name of All Platinum I summoners in KR region.
 P1 = lolstat_league_summoners('I','PLATINUM', 'RANKED_SOLO_5x5', 1, 'KR', APIKey)

}

}
