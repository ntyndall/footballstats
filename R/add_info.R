#' @title acommentary_info
#'
#' @description A function that takes a KEYS$COMP and matchID's, and
#'  determines general match statistics for both local team and visitor
#'  team
#'
#' @details Redis keys used;
#'   \itemize{
#'     \item{\strong{[SET]} :: \code{c_eventInSet:{comp_id}}}
#'     \item{\strong{[HASH]} :: \code{cme:{comp_id}:{match_id}:{event_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param matchIDs A character vector of matchIDs that match
#'  the matchEvents.
#' @param localteam An integer ID value representing the home team
#'  as defined by the API.
#' @param visitorteam An integer ID value representing the away team
#'  as defined by the API.
#'
#' @return Returns nothing, a redis hash map is set with the
#'  commentary information and IDs are stored as a redis set.
#'
#' @export


acommentary_info <- function(KEYS, matchIDs, localteam, visitorteam) {

  # Load static data set for testing
  if (KEYS$TEST) fullCommentary <- footballstats::fullCommentary

  for (i in 1:length(matchIDs)) {

    # Check if the commentary already exists for both teams
    rKeys <- paste0("cmt_commentary:", KEYS$COMP, ":", matchIDs[i], ":*") %>%
      rredis::redisKeys()
    if (rKeys %>% length %>% `==`(2)) next

    commentary <- if (KEYS$TEST) {
      fullCommentary[[i]]
    } else {  # nocov start
      footballstats::get_data(
        endpoint = paste0("/commentaries/", matchIDs[i], "?"),
        KEYS = KEYS
      )
    }  # nocov end

    localAway <- c('localteam', 'visitorteam')
    teamIDs <- c(localteam[i], visitorteam[i])

    # Make sure commentary exists
    if (commentary %>% is.null) next

    teamStats <- commentary$match_stats
    if (teamStats %>% length %>% `!=`(2)) next

    for (j in 1:length(localAway)) {
      singleTeamStats <- teamStats[[localAway[j]]]
      if (singleTeamStats %>% is.null) next

      rKey <- paste0("cmt_commentary:", KEYS$COMP, ":", matchIDs[i], ":", teamIDs[j])
      if (rKey %>% rredis::redisExists()) next

      # Add the commentary information
      footballstats::commentary_sub(
        competitionID = KEYS$COMP,
        matchID = matchIDs[i],
        teamID = teamIDs[j],
        teamStats = singleTeamStats,
        commentary = commentary$player_stats[[localAway[j]]]
      )
    }
  }
}


#' @title acomp_info
#'
#' @description A function that checks a unique competition has been
#'  obtained and is added to a hashMap of the form ....
#'  -> competitions:set
#'
#' @details API endpoints;
#'   \itemize{
#'     \item{\emph{"/competitions?Authorization={auth_id}"}}
#'   }
#'
#'  Redis Keys used;
#'   \itemize{
#'     \item{\strong{[SET]} :: \code{competition:set}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @return returns competitionIDs, a redis hash is set with season IDs, and a
#'  redis set is created to store the current seasonIDs.
#'
#' @export


acomp_info <- function(KEYS) {

  competitionIDs <- if (KEYS$TEST) {
    footballstats::compData
  } else {  # nocov start
    footballstats::get_data(
      endpoint = "/competitions?",
      KEYS = KEYS
    )
  }  # nocov end

  if (competitionIDs %>% is.null %>% `!`()) {
    total <- 0
    for (i in 1:nrow(competitionIDs)) {
      seasonID <- competitionIDs$id[[i]]
      compExists <- rredis::redisSAdd(
        set = 'competition:set',
        element = seasonID %>% as.character %>% charToRaw()
      )

      if (compExists == 1) total %<>% `+`(1)
    }
    cat(paste0(Sys.time(), ' | Successfully added ', total, ' new competition IDs to Redis. \n'))
    return(competitionIDs)
  }
}


#' @title acomp_standings
#'
#' @description A function that takes a KEYS$COMP and returns the current
#'  table information.
#'
#' @details API endpoints;
#'   \itemize{
#'     \item{\emph{"/standings/{comp_id}?Authorization={auth_id}"}}
#'   }
#'
#'  Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{comp:season:_standing_:{comp_id}:{season}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @return Returns nothing, a redis hash map is set with the competition
#'  standing information.
#'
#' @export


acomp_standings <- function(KEYS) {

  standings <- if (KEYS$TEST) {
    footballstats::standingData
  } else {  # nocov start
    footballstats::get_data(
      endpoint = paste0("/standings/", KEYS$COMP, "?"),
      KEYS = KEYS
    )
  }  # nocov end

  if (standings %>% is.null %>% `!`()) {
    for (i in 1:nrow(standings)) {
      singleTable <- standings[i, ]
      standingKey <- paste0("comp:season:_standing_:", KEYS$COMP, ':', singleTable$season)
      rredis::redisHMSet(
        key = standingKey,
        values = singleTable
      )
    }
  }
}


#' @title aevent_info
#'
#' @description A function that takes a KEYS$COMP, matchID's, and
#'  a data frame containing match event information to be split up and
#'  added to redis as single events.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[SET]} :: \code{c_eventInSet:{comp_id}}}
#'     \item{\strong{[HASH]} :: \code{cme:{comp_id}:{match_id}:{event_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param matchIDs An integer character vector of matchIDs that match
#'  the matchEvents.
#' @param matchEvents A list of data frames containing individual events
#'  grouped per match.
#'
#' @return Returns nothing, a redis hash map is set with the event information
#'  and IDs are stored as a redis set.
#'
#' @export


aevent_info <- function(KEYS, matchIDs, matchEvents) {
  for (i in 1:length(matchEvents)) {
    eventsPerMatch <- matchEvents[[i]]
    matchID <- matchIDs[i]

    # Check there are events in a match
    if (eventsPerMatch %>% length %>% `<`(1)) next
    for (j in 1:nrow(eventsPerMatch)) {
      event <- eventsPerMatch[j, ]
      inSet <- rredis::redisSAdd(
        set = paste0("c_eventInSet:", KEYS$COMP),
        element = event$id %>% as.character %>% charToRaw()
      ) %>% as.integer %>% as.logical

      # Make sure the event is new
      if (inSet %>% `!`()) next
      rredis::redisHMSet(
        key = paste0("cme:", KEYS$COMP, ":", matchID, ":", event$id),
        values = event
      )
    }
  }
}


#' @title amatch_info
#'
#' @description A function that takes a KEYS$COMP and season year to query
#'  for all the matches in a particular season and saves new teams to a set for
#'  later analysis.
#'
#' @details API endpoints;
#'   \itemize{
#'     \item{\emph{"/matches?comp_id={comp_id}&from_date={dd.mm.yyyy}&to_date={dd.mm.yyyy}&Authorization={auth_id}"}}
#'   }
#'
#'  Redis Keys used;
#'   \itemize{
#'     \item{\strong{[LIST]} :: \code{analyseTeams}}
#'     \item{\strong{[SET]} :: \code{c_matchSetInfo:{comp_id}}}
#'     \item{\strong{[SET]} :: \code{c:{comp_id}:ready}}
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'     \item{\strong{[HASH]} :: \code{c:{comp_id}:pred:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @return Returns a match dataframe containing all match information to update
#'  events in a particular match. Redis is updated with match information.
#' @return Returns a NULL dataframe if no matches are found.
#'
#' @export


amatch_info <- function(KEYS) {
  valuesToRetain <- c("id", "comp_id", "formatted_date", "season",
                      "week", "venue", "venue_id", "venue_city",
                      "status", "timer", "time", "localteam_id",
                      "localteam_name", "localteam_score", "visitorteam_id",
                      "visitorteam_name", "visitorteam_score", "ht_score",
                      "ft_score", "et_score", "penalty_local", "penalty_visitor")

  matches <-if (KEYS$TEST) {
    footballstats::matchData
  } else {  # nocov start
    footballstats::get_data(
      endpoint = paste0(
        "/matches?comp_id=", KEYS$COMP, "&from_date=", KEYS$DATE_FROM, "&to_date=", KEYS$DATE_TO, "&"),
      KEYS = KEYS
    )
  }  # nocov end

  if (matches %>% is.null %>% `!`()) {

    # If getting match info - make sure all matches have actually ended and been played!
    matches %<>% subset(matches$status %>% `==`('FT'))

    uniqueTeams <- c(matches$localteam_id, matches$visitorteam_id) %>% unique
    for (i in 1:(uniqueTeams %>% length)) {
      # Push teamID to List to analyse later
      rredis::redisLPush(
        key = 'analyseTeams',
        value = uniqueTeams[i] %>% as.character %>% charToRaw()
      )
    }

    # Loop over all match data
    for (i in 1:nrow(matches)) {
      single <- matches[i, ]
      matchItems <- single[ ,valuesToRetain]

      # Check if match belongs to set
      matchInSet <- rredis::redisSAdd(
        set = paste0('c_matchSetInfo:', KEYS$COMP),
        element = matchItems$id %>% as.character %>% charToRaw()
      ) %>% as.integer %>% as.logical

      if (matchInSet) {
        matchKey <- paste0(
          "csm:", matchItems$comp_id, ":",
          KEYS$SEASON, ":", matchItems$id
        )

        rredis::redisHMSet(
          key = matchKey,
          values = matchItems
        )

        # Push matches that have already been predicted to a set
        if (paste0('c:', KEYS$COMP, ':pred:', matchItems$id) %>% rredis::redisExists()) {
          rredis::redisSAdd(
            set = paste0('c:', KEYS$COMP, ':ready'),
            element = matchItems$id %>% as.character %>% charToRaw()
          )
        }
      }
    }
    return(matches)
  } else {
    return(data.frame(stringsAsFactors = FALSE))
  }
}


#' @title aplayer_info
#'
#' @description A function that takes a KEYS$COMP and length of players to
#'  analyse. The playerID's are popped from a Redis list and queried. The player
#'  stats are then stored in appropriate redis keys as necessary.
#'
#' @details API endpoints;
#'   \itemize{
#'     \item{\emph{"/player/{player_id}?Authorization={auth_id}"}}
#'   }
#'
#'  Redis Keys used;
#'   \itemize{
#'     \item{\strong{[LIST]} :: \code{analysePlayers}}
#'     \item{\strong{[HASH]} :: \code{ctps_**:{comp_id}:{team_id}:{player_id}:{season}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param playerLength An integer value that defines the number of players to
#'  analyse for a given list of ID's previously generated.
#'
#' @return Returns nothing. Redis is updated with player information
#'
#' @export


aplayer_info <- function(KEYS, playerLength) {
  valuesToRetain <- c("id", "common_name", "name", "firstname",
                      "lastname", "team", "teamid", "nationality",
                      "birthdate", "age", "birthcountry",
                      "birthplace", "position", "height", "weight")

  # Set the progress bar
  progressBar <- utils::txtProgressBar(
    min = 0,
    max = playerLength,
    style = 3
  )

  sapply(1:playerLength, function(i) {
    playerID <- 'analysePlayers' %>% rredis::redisLPop()

    playerData <- if (KEYS$TEST) {  # nocov start
      footballstats::playerData
    } else {
      footballstats::get_data(
        endpoint = paste0("/player/", playerID, "?"),
        KEYS = KEYS
      )
    }  # nocov end

    if (playerData %>% is.null %>% `!`()) {
      stats <- playerData$player_statistics
      statNames <- names(stats)
      sapply(1:length(statNames), function(j) {
        statData <- stats[[statNames[j]]]
        if (length(statData) != 0 || is.data.frame(statData)) {
          sapply(1:nrow(statData), function(k) {
            currentStat <- statData[k, ]
            season <- substr(currentStat$season, 1, 4)

            seasonInt <- ifelse(
              test = season %>% nchar %>% `==`(4),
              yes = season %>% as.integer,
              no = 0
            )

            if (seasonInt == KEYS$SEASON) {
              statKeyName <- paste0(
                'ctps_', statNames[j], ':', currentStat$league_id, ':',
                currentStat$id, ':', playerData$id, ':', season
              )
              rredis::redisHMSet(
                key = statKeyName,
                values = currentStat
              )
            }
          })
        }
      })
    }
    utils::setTxtProgressBar(progressBar, i)
  })
  close(progressBar)
}


#' @title ateam_info
#'
#' @description A function that takes a KEYS$COMP and integer value
#'  with details of the teamID list for analysis. Each team is
#'  queried by the API for relevant information and statistics are
#'  stored.
#'
#' @details API endpoints;
#'   \itemize{
#'     \item{\emph{"/team/{team_id}?Authorization={auth_id}"}}
#'   }
#'
#'  Redis Keys used;
#'   \itemize{
#'     \item{\strong{[SET]} :: \code{c_playerSetInfo}}
#'     \item{\strong{[LIST]} :: \code{analyseTeams}}
#'     \item{\strong{[LIST]} :: \code{analysePlayers}}
#'     \item{\strong{[HASH]} :: \code{ct_basics:{comp_id}:{team_id}}}
#'     \item{\strong{[HASH]} :: \code{ct_stats:{comp_id}:{team_id}}}
#'     \item{\strong{[HASH]} :: \code{ctp:{comp_id}:{team_id}:{player_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param teamListLength An integer value that defines how long the list
#'  containing teamID's is TeamID's are then popped from the list as they
#'  are anaylsed.
#'
#' @return Returns nothing. A Redis hash map is set with the team
#'  information.
#'
#' @export


ateam_info <- function(KEYS, teamListLength) {
  valuesToRetain <- c("team_id", "is_national", "name", "country",
                      "founded", "leagues", "venue_name", "venue_id",
                      "venue_surface", "venue_address", "venue_city",
                      "venue_capacity", "coach_name", "coach_id")

  # Set the progress bar
  progressBar <- utils::txtProgressBar(
    min = 0,
    max = teamListLength,
    style = 3
  )

  for (i in 1:teamListLength) {
    utils::setTxtProgressBar(progressBar, i)

    teamID <- 'analyseTeams' %>% rredis::redisLPop()

    teamData <- if (KEYS$TEST) {  # nocov start
      footballstats::teamData
    } else {
      footballstats::get_data(
        endpoint = paste0("/team/", teamID, "?"),
        KEYS = KEYS
      )
    }  # nocov end

    if (teamData %>% is.null) next

    basic <- paste0("ct_basic:", KEYS$COMP, ":", teamData$team_id)
    stats <- paste0("ct_stats:", KEYS$COMP, ":", teamData$team_id)
    squad <- paste0("ctp:", KEYS$COMP, ":", teamData$team_id)

    basicData <- teamData[valuesToRetain]
    rredis::redisHMSet(
      key = basic,
      values = basicData
    )

    squadInfo <- teamData$squad
    if (squadInfo %>% length %>% `<`(1)) next

    for (k in 1:nrow(squadInfo)) {
      playerID <- squadInfo$id[k]
      squadPlayer <- paste0(squad, ":", playerID)
      rredis::redisHMSet(
        key = squadPlayer,
        values = squadInfo[k, ]
      )

      # Check if player has been added to the set for analysis later.
      # Or if it is ready to be updated after another match has been played.
      newPlayers <- rredis::redisSAdd(
        set = paste0('c_playerSetInfo'),
        element = playerID %>% as.character() %>% charToRaw()
      ) %>% as.integer %>% as.logical

      if (newPlayers) {
        rredis::redisLPush(
          key = 'analysePlayers',
          value = playerID %>% as.character() %>% charToRaw()
        )
      }
    }

    rredis::redisHMSet(
      key = stats,
      values = teamData$statistics
    )

  }
  close(progressBar)
}

#' @title Commentary Sub-function
#'
#' @description A function that is an abstraction from \code{acommentary_info} that
#'  handles adding player statistics and general commentary information from a
#'  commentary list.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{cmt_commentary:{comp_id}:{match_id}:{team_id}}}
#'     \item{\strong{[HASH]} :: \code{cmp:{comp_id}:{match_id}:{player_id}}}
#'   }
#'
#' @param competitionID A character string representing the competitionID
#'  that is under investigation.
#' @param matchID An integer ID representing a single match
#'  as defined by the API.
#' @param teamID An integer ID value representing a single team
#'  as defined by the API.
#' @param teamStats A list object that contains the teams commentary
#'  statistics.
#' @param commentary A list object that contains multiple items, including
#'  the player information.
#'
#' @export


commentary_sub <- function(competitionID, matchID, teamID, teamStats, commentary) {
  rredis::redisHMSet(
    key = paste0("cmt_commentary:", competitionID, ":", matchID, ":", teamID),
    values = teamStats
  )
  playerStats <- commentary$player %>% purrr::when(
    is.null(.) ~ data.frame(), ~ .
  )

  # If any player stats exists then analyse them
  pRow <- playerStats %>% nrow
  if (pRow > 0) {
    for (j in 1:pRow) {
      rredis::redisHMSet(
        key = paste0("cmp:", competitionID, ":", matchID, ":", playerStats[j, ]$id),
        values = playerStats[j, ]
      )
    }
  }
}
