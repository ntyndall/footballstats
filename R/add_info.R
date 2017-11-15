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
#' @param matchIDs An integer character vector of matchIDs that match
#'  the matchEvents.
#'  
#' @return Returns nothing, a redis hash map is set with the 
#'  commentary information and IDs are stored as a redis set.
#'


acommentary_info <- function(competitionID, matchIDs, localteam, visitorteam, KEYS,
                             bypass = FALSE) {
  
  for (i in 1:length(matchIDs)) {
    if (rredis::redisExists(key = 'active')) {
      commentary <- footballstats::get_data(
        endpoint = paste0("/commentaries/", matchIDs[i], "?"),
        KEYS = KEYS)
      footballstats::request_limit()
    } else {
      print(Sys.time(), ' : Run out of requests in addCommentaryInfo()')
      commentary <- NULL
    }

    localAway <- c('localteam', 'visitorteam')
    teamIDs <- c(localteam[i], visitorteam[i])
    if (!is.null(commentary)) {
      teamStats <- commentary$match_stats
      if (length(teamStats) == 2) {
        for (j in 1:length(localAway)) {
          singleTeamStats <- teamStats[[localAway[j]]]
          if (!is.null(singleTeamStats)) {
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
#' @return returns nothing, a redis hash is set with season IDs, and a 
#'  redis set is created to store the current seasonIDs.


acomp_info <- function(KEYS, bypass = FALSE) {
  if (rredis::redisExists(key = 'active')) {
    competitionIDs <- footballstats::get_data(
      endpoint = "/competitions?",
      KEYS = KEYS)
    footballstats::request_limit()
  } else {
    print(Sys.time(), ' : Run out of requests in addCompetitionInfo()')
  }
  
  if (!is.null(competitionIDs)) {
    total <- 0
    for (i in 1:nrow(competitionIDs)) {
      seasonID <- competitionIDs$id[[i]]
      compExists <- rredis::redisSAdd(
        set = 'competition:set',
        element = seasonID)
      if (compExists == 1) {
        total <- total + 1
      }
    }
    print(paste0(Sys.time(), ' : Successfully added ', total, ' new competition IDs to Redis.'))
    #redisConnection$SET(key = 'competition:waitForNextQuery',
    #                    value = as.integer(Sys.Date() + daysUntilNextQuery))
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


acomp_standings <- function(competitionID, KEYS, bypass = FALSE) {
  if (rredis::redisExists(key = 'active')) {
    standings <- footballstats::get_data(
      endpoint = paste0("/standings/", competitionID, "?"),
      KEYS = KEYS)
    footballstats::request_limit()
  } else {
    print(Sys.time(), ' : Run out of requests in addCompetitionStandingInfo()')
    standings <- NULL
  }
  
  if (!is.null(standings)) {
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


aevent_info <- function(competitionID, matchIDs, matchEvents, bypass = FALSE) {
  for (i in 1:length(matchEvents)) {
    eventsPerMatch <- matchEvents[[i]]
    matchID <- matchIDs[i]
    if (length(eventsPerMatch) > 0) {
      for (j in 1:nrow(eventsPerMatch)) {
        event <- eventsPerMatch[j, ]
        inSet <- rredis::redisSAdd(
          set = paste0("c_eventInSet:", competitionID),
          element = event$id)
        if (as.logical(as.integer(inSet))) {
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
#' @param updateData A boolean that is set to TRUE if team data is to be analysed
#'  again, i.e. after a match. FALSE to ignore and only analyse new teams. Generally
#'  set to FALSE for first time run.
#' @param analysingToday A boolean that is set to TRUE if data is being analysing today.
#'  This is used to figure out if matches have been played during the time of 
#'  query, if not then wait until todays match has been played.
#'  
#' @return Returns a match dataframe containing all match information to update 
#'  events in a particular match. Redis is updated with match information.
#' @return Returns a NULL dataframe if no matches are found.
#'


amatch_info <- function(competitionID, dateFrom, dateTo, seasonStarting, updateData, 
                        analysingToday = TRUE, KEYS, bypass = FALSE) {
  valuesToRetain <- c("id", "comp_id", "formatted_date", "season",           
                      "week", "venue", "venue_id", "venue_city",     
                      "status", "timer", "time", "localteam_id",  
                      "localteam_name", "localteam_score", "visitorteam_id",
                      "visitorteam_name", "visitorteam_score", "ht_score",
                      "ft_score", "et_score", "penalty_local", "penalty_visitor")
  
  if (bypass) {
    ## Load data here
    data(matchData, package = 'footballstats', envir = .GlobalEnv)
    matches <- matchData
  } else {  
    matches <- footballstats::get_data(
      endpoint = paste0(
        "/matches?comp_id=", competitionID, "&from_date=", dateFrom, "&to_date=", dateTo, "&"),
      KEYS = KEYS)
    footballstats::request_limit()
  }

  if (!is.null(matches)) {
    
    # If getting todays match information, make sure all matches have actually been played.
    if (analysingToday) {
      if (any(matches$localteam_score == "")) {
        return(data.frame(stringsAsFactors = FALSE))
      }
    }
    
    # Loop over all match data
    for (i in 1:nrow(matches)) {
      single <- matches[i, ]
      matchItems <- single[ ,valuesToRetain] 
      
      # Check if team has been added to the set for analysis later.
      # Or if it is ready to be updated after another match has been played.
      teamInSet <- rredis::redisSAdd(
        set = paste0('c_teamSetInfo:', competitionID),
        element = matchItems$localteam_id)
      
      if (as.logical(as.integer(teamInSet)) || updateData) {
        rredis::redisLPush(
          key = 'analyseTeams', 
          value = matchItems$localteam_id)
      }
      
      # Check if match belongs to set
      matchInSet <- rredis::redisSAdd(
        set = paste0('c_matchSetInfo:', competitionID),
        element = matchItems$id)
      
      if (as.logical(as.integer(matchInSet))) {
        matchKey <- paste0(
          "csm:", matchItems$comp_id, ":", 
          seasonStarting, ":", matchItems$id)

        rredis::redisHMSet(
          key = matchKey, 
          values = matchItems)
        
        if (rredis::redisExists(key = paste0('c:', competitionID, ':pred:', matchItems$id))) {
          rredis::redisSAdd(
            set = paste0('c:', competitionID, ':ready'),
            element = matchItems$id)
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
#' @param competitionID An integer containing the competitionID that the 
#'  teams and match information belong to.
#' @param playerLength An integer value that defines the number of players to
#'  analyse for a given list of ID's previously generated.
#'  
#' @return Returns nothing. Redis is updated with player information
#'


aplayer_info <- function(competitionID, playerLength, currentSeasonYear, 
                         KEYS, bypass = FALSE) {
  valuesToRetain <- c("id", "common_name", "name", "firstname",
                      "lastname", "team", "teamid", "nationality",
                      "birthdate", "age", "birthcountry",
                      "birthplace", "position", "height", "weight")

  progressBar <- txtProgressBar(min = 0, max = playerLength, style = 3)
  sapply(1:playerLength, function(i) {
    if (rredis::redisExists(key = 'active')) {
      playerID <- rredis::redisLPop(
        key = 'analysePlayers')
      playerData <- footballstats::get_data(
        endpoint = paste0("/player/", playerID, "?"),
        KEYS = KEYS)
      footballstats::request_limit()
    } else {
      print(Sys.time(), ' : Run out of requests in addPlayerInfo()')
      playerData <- NULL
    }
    
    if (!is.null(playerData)) {
      stats <- playerData$player_statistics
      statNames <- names(stats)
      sapply(1:length(statNames), function(j) {
        statData <- stats[[statNames[j]]]
        if (length(statData) != 0 || is.data.frame(statData)) {
          sapply(1:nrow(statData), function(k) {
            currentStat <- statData[k, ]
            season <- substr(currentStat$season, 1, 4)
            
            if (nchar(season) == 4) {
              seasonInt <- as.integer(season)
            } else {
              seasonInt <- 0
            }
            
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
    setTxtProgressBar(progressBar, i)
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
#' @param updateData A boolean that is set to TRUE if team data is to be analysed
#'  again, i.e. after a match. FALSE to ignore and only analyse new teams. Generally
#'  set to FALSE for first time run.
#'  
#' @return Returns nothing. A Redis hash map is set with the team
#'  information. 
#'


ateam_info <- function(competitionID, teamListLength, updateData,
                       KEYS, bypass = FALSE) {
  valuesToRetain <- c("team_id", "is_national", "name", "country",
                      "founded", "leagues", "venue_name", "venue_id",
                      "venue_surface", "venue_address", "venue_city",
                      "venue_capacity", "coach_name", "coach_id")
  
  for (i in 1:teamListLength) {
    if (rredis::redisExists(key = 'active')) {
      teamID <- rredis::redisLPop(
        key = 'analyseTeams')
      teamData <- footballstats::get_data(
        endpoint = paste0( "/team/", teamID, "?"),
        KEYS = KEYS)
      footballstats::request_limit()
    } else {
      print(Sys.time(), ' : Run out of requests in addTeamInfo()')
      teamData <- NULL
    }
    
    if(!is.null(teamData)) {
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
            element = playerID)
          
          if (as.logical(as.integer(newPlayers)) || updateData) {
            rredis::redisLPush(
              key = 'analysePlayers', 
              value = playerID)
          }
        }
      }
      rredis::redisHMSet(
        key = stats, 
        values = teamData$statistics)
    }
  }
}

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
        values = playerStats)
    }
  }  
}
