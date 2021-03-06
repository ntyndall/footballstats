% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_league_table.R
\name{create_table}
\alias{create_table}
\title{Create weekly points}
\usage{
create_table(KEYS, matchData)
}
\arguments{
\item{matchData}{A data frame that contains rows of single matches
that have been played between two teams.}
}
\value{
Nothing. The points and goal information is stored in redis.
}
\description{
A function that takes a data frame of matchData and recreates
 the league table by adding points on a weekly basis based on the date
 timestamps of the data frame.

 Make sure the match data has been recreated, i.e. it needs to be stored
 in redis, then ordered by date to work.
}
\details{
Redis Keys used;
  \itemize{
    \item{\strong{[KEY]} :: \code{c_startDate:{comp_id}:{season}}}
    \item{\strong{[SET]} :: \code{leagueMatchSet}}
    \item{\strong{[HASH]} :: \code{cwt_l:{comp_id}:{season}:{week#}:{team_id}}}
  }
}
