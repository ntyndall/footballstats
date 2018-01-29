#' @title acommentary_info
#'
#' @description A function that takes a competitionID and matchID's, and
#'  determines general match statistics for both local team and visitor
#'  team
#'
#' @details EventID's are checked if they have been analysed already;
#'     ->   [c_eventInSet]:{comp_id}   ->   [SET]
#'  The actual event information is stored as a hash map as;
#'     ->   [cme]:{comp_id}:{match_id}:{event_id}  ->   [HASH]
#'
#' @param competitionID An integer defining the competitionID that the
#'  team belongs to.
#' @param matchIDs A character vector of matchIDs that match
#'  the matchEvents.
#'
#' @return Returns nothing, a redis hash map is set with the
#'  commentary information and IDs are stored as a redis set.
#'
#' @export


acommentary_info <- function(competitionID, matchIDs, localteam, visitorteam, KEYS,
                             bypass = FALSE) {

  # Load static data set for testing
  if (KEYS$TEST) fullCommentary <- footballstats::fullCommentary

  for (i in 1:length(matchIDs)) {
    if (KEYS$TEST) {
      commentary <- fullCommentary[[i]]
    } else {  # nocov start
      commentary <- footballstats::get_data(
        endpoint = paste0("/commentaries/", matchIDs[i], "?"),
        KEYS = KEYS)
      footballstats::request_limit()
    }  # nocov end

    localAway <- c('localteam', 'visitorteam')
    teamIDs <- c(localteam[i], visitorteam[i])

    if (commentary %>% is.null %>% `!`()) {
      teamStats <- commentary$match_stats
      if (length(teamStats) == 2) {
        for (j in 1:length(localAway)) {
          singleTeamStats <- teamStats[[localAway[j]]]
          if (singleTeamStats %>% is.null %>% `!`()) {
            footballstats::commentary_sub(
              competitionID = competitionID,
              matchID = matchIDs[i],
              teamInfo = teamIDs[j],
              teamStats = singleTeamStats,
              commentary = commentary$player_stats[[localAway[j]]])
          }
        }
      }
    }
  }
}


#' @title acomp_info
#'
#' @description A function that checks a unique competition has been
#'  obtained and is added to a hashMap of the form ....
#'  -> competitions:set
#'
#' @param production A boolean to indicate whether production (default)
#'  runs are performed or testing carried out.
#' @param seasonIDs A list containing seasonIDs...
#'
#' @return returns competitionIDs, a redis hash is set with season IDs, and a
#'  redis set is created to store the current seasonIDs.
#'
#' @export


acomp_info <- function(KEYS) {

  if (KEYS$TEST) {
    competitionIDs <- footballstats::compData
  } else {  # nocov start
    competitionIDs <- footballstats::get_data(
      endpoint = "/competitions?",
      KEYS = KEYS)
    footballstats::request_limit()
  }  # nocov end

  if (competitionIDs %>% is.null %>% `!`()) {
    total <- 0
    for (i in 1:nrow(competitionIDs)) {
      seasonID <- competitionIDs$id[[i]]
      compExists <- rredis::redisSAdd(
        set = 'competition:set',
        element = seasonID %>% as.character %>% charToRaw())

      if (compExists == 1) total %<>% `+`(1)
    }
    cat(paste0(Sys.time(), ' | Successfully added ', total, ' new competition IDs to Redis. \n'))
    return(competitionIDs)
  }
}


#' @title acomp_standings
#'
#' @description A function that takes a competitionID and returns the current
#'  table information.
#'
#' @details Competition table information is stored in the following redis hash
#'    ->   comp:season:_standing_:{comp_id}:{season}
#'
#' @param competitionID An integer containing the competition ID that the
#'  teams and match information belong to.
#'
#' @return Returns nothing, a redis hash map is set with the competition
#'  standing information.
#'
#' @export


acomp_standings <- function(competitionID, KEYS) {

  if (KEYS$TEST) {
    standings <- footballstats::standingData
  } else {  # nocov start
    standings <- footballstats::get_data(
      endpoint = paste0("/standings/", competitionID, "?"),
      KEYS = KEYS)
    footballstats::request_limit()
  }  # nocov end

  if (standings %>% is.null %>% `!`()) {
    for (i in 1:nrow(standings)) {
      singleTable <- standings[i, ]
      standingKey <- paste0(
        "comp:season:_standing_:", competitionID, singleTable$season)
      rredis::redisHMSet(
        key = standingKey,
        values = singleTable
      )
    }
  }
}


#' @title aevent_info
#'
#' @description A function that takes a competitionID, matchID's, and
#'  a data frame containing match event information to be split up and
#'  added to redis as single events.
#'
#' @details EventID's are checked if they have been analysed already;
#'     ->   [c_eventInSet]:{comp_id}   ->   [SET]
#'  The actual event information is stored as a hash map as;
#'     ->   [cme]:{comp_id}:{match_id}:{event_id}  ->   [HASH]
#'
#' @param competitionID An integer defining the competitionID that the
#'  team belongs to.
#' @param matchIDs An integer character vector of matchIDs that match
#'  the matchEvents.
#' @param matchEvents A list of data frames containing individual events
#'  grouped per match.
#'
#' @return Returns nothing, a redis hash map is set with the event information
#'  and IDs are stored as a redis set.
#'
#' @export


aevent_info <- function(competitionID, matchIDs, matchEvents) {
  for (i in 1:length(matchEvents)) {
    eventsPerMatch <- matchEvents[[i]]
    matchID <- matchIDs[i]
    if (length(eventsPerMatch) > 0) {
      for (j in 1:nrow(eventsPerMatch)) {
        event <- eventsPerMatch[j, ]
        inSet <- rredis::redisSAdd(
          set = paste0("c_eventInSet:", competitionID),
          element = event$id %>% as.character %>% charToRaw()) %>%
            as.integer %>%
            as.logical
        if (inSet) {
          rredis::redisHMSet(
            key = paste0("cme:", competitionID, ":", matchID, ":", event$id),
            values = event)
        }
      }
    }
  }
}


#' @title amatch_info
#'
#' @description A function that takes a competitionID and season year to query
#'  for all the matches in a particular season and saves new teams to a set for
#'  later analysis.
#'
#' @details Match information is stored in a hash map as;
#'     ->   [csm]:{comp_id}:{season}:{match_id}   ->   [HASH]
#'  The matches involved are first checked to see if they already exist
#'  in redis by checking the set;
#'     ->   [c_matchSetInfo]:{comp_id}   ->   [SET]
#'  The teams involved in the match are checked to see if they are new,
#'  by checking the set in redis;
#'     ->   [c_teamSetInfo]:{comp_id}   ->   [SET]
#'
#' @param competitionID An integer containing the competitionID that the
#'  teams and match information belong to.
#' @param dateFrom A POSIXct value converted to dd.mm.yyyy format which denotes
#'  the start date for querying the API.
#' @param dateTo A POSIXct value converted to dd.mm.yyyy format which denotes
#'  the end date for querying the API.
#' @param analysingToday A boolean that is set to TRUE if data is being analysing today.
#'  This is used to figure out if matches have been played during the time of
#'  query, if not then wait until todays match has been played.
#'
#' @return Returns a match dataframe containing all match information to update
#'  events in a particular match. Redis is updated with match information.
#' @return Returns a NULL dataframe if no matches are found.
#'
#' @export


amatch_info <- function(competitionID, dateFrom, dateTo, seasonStarting,
                        analysingToday = TRUE, KEYS) {
  valuesToRetain <- c("id", "comp_id", "formatted_date", "season",
                      "week", "venue", "venue_id", "venue_city",
                      "status", "timer", "time", "localteam_id",
                      "localteam_name", "localteam_score", "visitorteam_id",
                      "visitorteam_name", "visitorteam_score", "ht_score",
                      "ft_score", "et_score", "penalty_local", "penalty_visitor")

  if (KEYS$TEST) {
    matches <- footballstats::matchData
  } else {  # nocov start
    matches <- footballstats::get_data(
      endpoint = paste0(
        "/matches?comp_id=", competitionID, "&from_date=", dateFrom, "&to_date=", dateTo, "&"),
      KEYS = KEYS)
    footballstats::request_limit()
  }  # nocov end

  if (matches %>% is.null %>% `!`()) {

    # If getting match info - make sure all matches have actually ended and been played!
    matches %<>% subset(matches$status %>% `==`('FT'))

    # Loop over all match data
    for (i in 1:nrow(matches)) {
      single <- matches[i, ]
      matchItems <- single[ ,valuesToRetain]

      # Push teamID to List to analyse later
      rredis::redisLPush(
        key = 'analyseTeams',
        value = matchItems$localteam_id %>% as.character %>% charToRaw())

      # Check if match belongs to set
      matchInSet <- rredis::redisSAdd(
        set = paste0('c_matchSetInfo:', competitionID),
        element = matchItems$id %>% as.character %>% charToRaw()) %>%
          as.integer %>% as.logical

      if (matchInSet) {
        matchKey <- paste0(
          "csm:", matchItems$comp_id, ":",
          seasonStarting, ":", matchItems$id)

        rredis::redisHMSet(
          key = matchKey,
          values = matchItems)

        # Push matches that have already been predicted to a set
        if (rredis::redisExists(key = paste0('c:', competitionID, ':pred:', matchItems$id))) {
          rredis::redisSAdd(
            set = paste0('c:', competitionID, ':ready'),
            element = matchItems$id %>% as.character %>% charToRaw())
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
#' @description A function that takes a competitionID and length of players to
#'  analyse. The playerID's are popped from a Redis list and queried. The player
#'  stats are then stored in appropriate redis keys as necessary.
#'
#' @details Player stats infromation is stored in a hash map as;
#'     ->   [ctps]:{comp_id}:{team_id}:{player_id}:{season}:[_stats_`statType`_]   ->   [HASH]
#'
#' @param playerLength An integer value that defines the number of players to
#'  analyse for a given list of ID's previously generated.
#'
#' @return Returns nothing. Redis is updated with player information
#'
#' @export


aplayer_info <- function(playerLength, currentSeasonYear, KEYS) {
  valuesToRetain <- c("id", "common_name", "name", "firstname",
                      "lastname", "team", "teamid", "nationality",
                      "birthdate", "age", "birthcountry",
                      "birthplace", "position", "height", "weight")

  # Set the progress bar
  progressBar <- utils::txtProgressBar(
    min = 0,
    max = playerLength,
    style = 3)

  sapply(1:playerLength, function(i) {
    playerID <- 'analysePlayers' %>% rredis::redisLPop()

    if (KEYS$TEST) {  # nocov start
      playerData <- footballstats::playerData
    } else {
      playerData <- footballstats::get_data(
        endpoint = paste0("/player/", playerID, "?"),
        KEYS = KEYS)
      footballstats::request_limit()
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
              test = nchar(season) == 4,
              yes = season %>% as.integer(),
              no = 0)

            if (seasonInt == currentSeasonYear) {
              statKeyName <- paste0(
                'ctps_', statNames[j], ':', currentStat$league_id, ':',
                currentStat$id, ':', playerData$id, ':', season)
              rredis::redisHMSet(
                key = statKeyName,
                values = currentStat)
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
#' @description A function that takes a competitionID and integer value
#'  with details of the teamID list for analysis. Each team is
#'  queried by the API for relevant information and statistics are
#'  stored.
#'
#' @details A number of information is stored.
#'  The basic information is stored as;
#'     ->   [ct_basic]:{comp_id}:{team_id}   ->   [HASH]
#'  The team statistics is stored as;
#'     ->   [ct_stats]:{comp_id}:{team_id}   ->   [HASH]
#'  Player information relevent to the team is stored as;
#'     ->   [ctp]:{comp_id}:{team_id}:{player_id}   ->   [HASH]
#'
#' @param competitionID An integer defining the competitionID that the
#'  team belongs to.
#' @param teamListLength An integer value that defines how long the list
#'  containing teamID's is TeamID's are then popped from the list as they
#'  are anaylsed.
#'
#' @return Returns nothing. A Redis hash map is set with the team
#'  information.
#'
#' @export


ateam_info <- function(competitionID, teamListLength, KEYS) {
  valuesToRetain <- c("team_id", "is_national", "name", "country",
                      "founded", "leagues", "venue_name", "venue_id",
                      "venue_surface", "venue_address", "venue_city",
                      "venue_capacity", "coach_name", "coach_id")

  for (i in 1:teamListLength) {
    if (KEYS$TEST) {  # nocov start
      teamData <- footballstats::teamData
    } else {
      teamID <- 'analyseTeams' %>% rredis::redisLPop()
      teamData <- footballstats::get_data(
        endpoint = paste0( "/team/", teamID, "?"),
        KEYS = KEYS)
      footballstats::request_limit()
    }  # nocov end

    if (teamData %>% is.null %>% `!`()) {
      basic <- paste0("ct_basic:", competitionID, ":", teamData$team_id)
      stats <- paste0("ct_stats:", competitionID, ":", teamData$team_id)
      squad <- paste0("ctp:", competitionID, ":", teamData$team_id)

      basicData <- teamData[valuesToRetain]
      rredis::redisHMSet(
        key = basic,
        values = basicData)

      squadInfo <- teamData$squad
      if (length(squadInfo) > 0) {
        for (k in 1:nrow(squadInfo)) {
          playerID <- squadInfo$id[k]
          squadPlayer <- paste0(squad, ":", playerID)
          rredis::redisHMSet(
            key = squadPlayer,
            values = squadInfo[k, ])

          # Check if player has been added to the set for analysis later.
          # Or if it is ready to be updated after another match has been played.
          newPlayers <- rredis::redisSAdd(
            set = paste0('c_playerSetInfo'),
            element = playerID %>% as.character() %>% charToRaw()) %>%
              as.integer %>% as.logical

          if (newPlayers) {
            rredis::redisLPush(
              key = 'analysePlayers',
              value = playerID %>% as.character() %>% charToRaw())
          }
        }
      }
      rredis::redisHMSet(
        key = stats,
        values = teamData$statistics)
    }
  }
}

#' @title Commentary Sub-function
#' @export


commentary_sub <- function(competitionID, matchID, teamInfo, teamStats, commentary) {
  rredis::redisHMSet(
    key = paste0("cmt_commentary:", competitionID, ":", matchID, ":", teamInfo),
    values = teamStats)
  playerStats <- commentary$player %>%
    purrr::when(is.null(.) ~ data.frame(), ~ .)
  if (nrow(playerStats) > 0) {
    for (j in 1:nrow(playerStats)) {
      rredis::redisHMSet(
        key = paste0("cmp:", competitionID, ":", matchID, ":", playerStats[j, ]$id),
        values = playerStats[j, ])
    }
  }
}
