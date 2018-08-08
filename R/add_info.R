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

  # Check if the commentary already exists for both teams for all IDs
  comExists <- KEYS$RED$pipeline(
    .commands = lapply(
      X = paste0("cmt_commentary:", KEYS$COMP, ":", matchIDs, ":*"),
      FUN = function(x) x %>% KEYS$PIPE$KEYS()
    )
  ) %>%
    purrr::map(length) %>%
    purrr::flatten_int() %>%
    `!=`(2)

  # I should also check if the lineups exist too
  # ...

  # Subset down to what needs analysed
  if (comExists %>% any) {
    matchIDs %<>% `[`(comExists)
    localteam %<>% `[`(comExists)
    visitorteam %<>% `[`(comExists)

    # Get all commentaries here and only take the non-null elements
    commentaries <- if (KEYS$TEST) {
      footballstats::fullCommentary[[1]] %>% list
    } else {
      lapply(
        X = paste0("/commentaries/", matchIDs, "?"),
        FUN = function(x) x %>% footballstats::get_data(
          KEYS = KEYS
        )
      )
    }

    # Figure out which commentaries are null
    nullCom <- commentaries %>%
      purrr::map(is.null) %>%
      purrr::flatten_lgl()

    # If any are null then subset everything
    if (nullCom %>% any) {
      nullCom %<>% `!`()
      matchIDs %<>% `[`(nullCom)
      localteam %<>% `[`(nullCom)
      visitorteam %<>% `[`(nullCom)
      commentaries %<>% purrr::compact() # Compact the NULL's out
    }

    # Define named types of match commentaries
    localAway <- c('localteam', 'visitorteam')

    # ... continue on here
    if (commentaries %>% length %>% `>`(0)) {
      sapply(
        X = 1:(commentaries %>% length),
        FUN = function(x) {
          teamStats <- commentaries[[x]]$match_stats
          if (teamStats %>% length %>% `==`(2)) {
            teamIDs <- c(localteam[x], visitorteam[x])
            # Loop over both teams

            # Add the commentary information
            sapply(
              X = 1:2,
              FUN = function(z) {
                singleTeamStats <- teamStats[[localAway[z]]]
                if (singleTeamStats %>% is.null %>% `!`()) {
                  KEYS %>% footballstats::commentary_sub(
                    matchID = matchIDs[x],
                    teamID = teamIDs[z],
                    teamStats = singleTeamStats,
                    commentary = commentaries[[x]]$player_stats[[localAway[z]]]
                  )
                }
              }
            )
          }
        }
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

  comps <- if (KEYS$TEST) {
    footballstats::compData
  } else {  # nocov start
    footballstats::get_data(
      endpoint = "/competitions?",
      KEYS = KEYS
    )
  }  # nocov end

  # If competitions exist then add them to redis
  if (comps %>% is.null %>% `!`()) {
    # Add all ID's to set
    cAdded <- KEYS$RED$pipeline(
      .commands = lapply(
        X = comps$id,
        FUN = function(x) "competition:set" %>% KEYS$PIPE$SADD(x)
      )
    ) %>%
      purrr::flatten_int() %>%
      as.logical %>%
      sum

    if (KEYS$LOGGING) cat(paste0(Sys.time(), ' | Successfully added ', cAdded, ' new competition IDs to Redis. \n'))
    return(comps)
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

  # Set up the key name
  kName <- paste0("c_eventInSet:", KEYS$COMP, ":", KEYS$SEASON)

  # Are any events null?
  eNull <- matchEvents %>%
    purrr::map(is.null) %>%
    purrr::flatten_lgl()

  # Filter out just in case
  if (eNull %>% any) {
    matchIDs %<>% `[`(eNull %>% `!`())
    matchEvents %<>% purrr::compact()
  }

  if (eNull %>% all %>% `!`()) {
    # Get all the eventIDs
    allEventIDs <- matchEvents %>%
      purrr::map(`[`('id'))

    lapply(
      X = 1:(allEventIDs %>% length),
      FUN = function(x) {

        # Have the ID's been added?
        alreadyAdded <- KEYS$RED$pipeline(
          .commands = lapply(
            X = allEventIDs[[x]],
            FUN = function(y) kName %>% KEYS$PIPE$SADD(y)
          )
        ) %>%
          purrr::flatten_int() %>%
          as.logical

        # Subset those that have been added for the first time
        if (alreadyAdded %>% any) {
          # Subset the eventIDs
          eventIDs <- allEventIDs[[x]] %>%
            `[`(alreadyAdded)

          # Subset the events
          events <- matchEvents[[x]] %>%
            subset(alreadyAdded) %>%
            lapply(as.character)

          # event title names
          eventTitles <- events %>% names

          # Set up redis keys
          rKeys <- paste0("cme:", KEYS$COMP, ":", matchIDs[x], ":", eventIDs)

          # Add to redis
          KEYS$RED$pipeline(
            .commands = lapply(
              X = 1:(eventIDs %>% length),
              FUN = function(y) {
                rKeys[y] %>% KEYS$PIPE$HMSET(
                  field = eventTitles,
                  value = events %>% purrr::map(y) %>% as.character
                )
              }
            )
          )
        }
      }
    )
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
  valuesToRetain <- c(
    "id", "comp_id", "formatted_date", "season",
    "week", "venue", "venue_id", "venue_city",
    "status", "timer", "time", "localteam_id",
    "localteam_name", "localteam_score", "visitorteam_id",
    "visitorteam_name", "visitorteam_score", "ht_score",
    "ft_score", "et_score", "penalty_local", "penalty_visitor"
  )

  matches <- if (KEYS$TEST) {
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

    # Push unique team ID's to a list for analysis later
    KEYS$RED$pipeline(
      .commands = lapply(
        X = c(matches$localteam_id, matches$visitorteam_id) %>% unique,
        FUN = function(x) "analyseTeams" %>% KEYS$PIPE$LPUSH(x)
      )
    )

    # Define all the matchIDs
    matchIDs <- matches$id

    # Push matches that have already been predicted to a set
    predictionsExist <- KEYS$RED$pipeline(
      .commands = lapply(
        X = paste0('c:', KEYS$COMP, ':pred:', matchIDs),
        FUN = function(x) x %>% KEYS$PIPE$EXISTS()
      )
    ) %>%
      purrr::flatten_int() %>%
      as.logical

    # Push to a `ready` set for other functions to pick up
    if (predictionsExist %>% any) {
      paste0('c:', KEYS$COMP, ':ready') %>% KEYS$RED$SADD(
        member = matchIDs %>% `[`(predictionsExist)
      )
    }

    # See if any matches belong to the set already analysed
    addMatches <- KEYS$RED$pipeline(
      .commands = lapply(
        X = matches$id,
        FUN = function(x) paste0('c_matchSetInfo:', KEYS$COMP) %>% KEYS$PIPE$SADD(x)
      )
    ) %>%
      purrr::flatten_int() %>%
      as.logical

    # If any addMatches then subset the data frame
    if (addMatches %>% any) {
      # Only those that haven't been added
      matchesToAdd <- matches %>% subset(
        subset = addMatches,
        select = valuesToRetain
      )

      # Define the redis matchKey
      matchKeys <- paste0(
        "csm:", KEYS$COMP, ":",
        KEYS$SEASON, ":", matchesToAdd$id
      )

      # Push data to redis
      KEYS$RED$pipeline(
        .commands = lapply(
          X = 1:(matchKeys %>% length),
          FUN = function(x) matchKeys[x] %>% KEYS$PIPE$HMSET(
            field = valuesToRetain,
            value = matchesToAdd[x, ] %>% as.character
          )
        )
      )
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
  valuesToRetain <- c(
    "id", "common_name", "name", "firstname",
    "lastname", "team", "teamid", "nationality",
    "birthdate", "age", "birthcountry",
    "birthplace", "position", "height", "weight"
  )

  # Get all the player ID's
  playerIDs <- KEYS$RED$pipeline(
    .commands = lapply(
      X = 1:playerLength,
      FUN = function(x) "analysePlayers" %>% KEYS$PIPE$LPOP()
    )
  ) %>%
    purrr::flatten_chr()

  # Get the matching data set
  playerData <- if (KEYS$TEST) {  # nocov start
    footballstats::playerData %>% list
  } else {
    lapply(
      X =  paste0("/player/", playerID, "?"),
      FUN = function(x) x %>% footballstats::get_data(
        KEYS = KEYS
      )
    )
  }  # nocov end

  # Make sure no player information is null
  pIndex <- playerData %>%
    purrr::map(is.null) %>%
    purrr::flatten_lgl()

  if (pIndex %>% any) {
    playerIDs %<>% `[`(pIndex %>% `!`())
    playerData %<>% purrr::compact()
  }

  # If data exists then analyse it
  if (playerData %>% length %>% `>`(0)) {

    lapply(
      X = 1:(playerData %>% length),
      FUN = function(x) {

        # Set up basic information
        playerStats <- playerData[[x]]$player_statistics %>%
          purrr::compact()
        playerTypes <- playerStats %>%
          names
        playerID <- playerIDs[x]

        lapply(
          X = 1:(playerTypes %>% length),
          FUN = function(y) {
            cData <- playerStats %>% `[[`(playerTypes[y])

            # Get matching seasons
            cData %<>% subset(cData$season %>% substr(start = 1, stop = 4) %>% `==`(KEYS$SEASON))

            # How many data Rows
            datRows <- cData %>% nrow

            # If there are any then add the information to redis
            if (datRows %>% `>`(0)) {

              # Get all the key names (before flattening data frame)
              keyNames <- paste0(
                'ctps_', playerTypes[y], ':', cData$league_id, ':',
                cData$id, ':', playerID, ':', KEYS$SEASON
              )

              # Insert all redis hash information
              cData %<>% lapply(as.character)
              KEYS$RED$pipeline(
                .commands = lapply(
                  X = 1:datRows,
                  FUN = function(z) {
                    keyNames[z] %>% KEYS$PIPE$HMSET(
                      field = cData %>% names,
                      value = cData %>% purrr::map(z)
                    )
                  }
                )
              )
            }
          }
        )
      }
    )
  }
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
  valuesToRetain <- c(
    "team_id", "is_national", "name", "country",
    "founded", "leagues", "venue_name", "venue_id",
    "venue_surface", "venue_address", "venue_city",
    "venue_capacity", "coach_name", "coach_id"
  )

  # Pop off all TeamIDs
  teamIDs <- KEYS$RED$pipeline(
    .commands = lapply(
      X = 1:teamListLength,
      FUN = function(x) "analyseTeams" %>% KEYS$PIPE$LPOP()
    )
  ) %>%
    purrr::flatten_chr()

  teamData <- if (KEYS$TEST) {  # nocov start
    footballstats::teamData %>% list
  } else {
    lapply(
      X = paste0("/team/", teamIDs, "?"),
      FUN = function(x) x %>% footballstats::get_data(
        KEYS = KEYS
      )
    )
  }  # nocov end

  # Filter out nulls
  tIndex <- teamData %>%
    purrr::map(is.null) %>%
    purrr::flatten_lgl()

  # Realign indexes
  if (tIndex %>% any) {
    tIndex %<>% `!`() # Reverse for subsetting
    teamIDs %>% `[`(tIndex)
    teamData %<>% purrr::compact()
  }

  # Analyse data
  if (teamData %>% length %>% `>`(0)) {
    kNames <- lapply(
      X = teamIDs,
      FUN = function(x) {
        paste0(c("ct_basic:", "ct_stats:", "ctp:"), KEYS$COMP, ":", x)
      }
    )

    # Map + flatten
    mf <- function(x, y) x %>% purrr::map(y) %>% purrr::flatten_chr()

    # A named list of all the keys to be inserted
    allKeys <- list(
      basic = kNames %>% mf(1),
      stats = kNames %>% mf(2),
      squad = kNames %>% mf(3)
    )

    # 1) Push all basic data first (subset the values to retain)
    tBasic <- teamData %>%
      purrr::map(function(x) x %>% `[`(valuesToRetain) %>% as.character)

    KEYS$RED$pipeline(
      .commands = lapply(
        X = 1:(tBasic %>% length),
        FUN = function(x) {
          allKeys$basic[x] %>% KEYS$PIPE$HMSET(
            field = valuesToRetain,
            value = tBasic[[x]]
          )
        }
      )
    )

    # 2) Push all statistics data
    tStats <- teamData %>%
      purrr::map(function(x) x %>% `[[`('statistics'))

    # Just make sure none are NULL
    sNull <- tStats %>%
      purrr::map(is.null) %>%
      purrr::flatten_lgl()

    if (sNull %>% any) {
      sNull %<>% `!`()
      allKeys$stats %<>% `[`(sNull)
      tStats %<>% purrr::compact()
    }

    # Then push all statistics
    if (tStats %>% length %>% `>`(0)) {
      KEYS$RED$pipeline(
        .commands = lapply(
          X = 1:(tStats %>% length),
          FUN = function(x) {
            allKeys$stats[x] %>% KEYS$PIPE$HMSET(
              field = tStats[[x]] %>% names,
              value = tStats[[x]] %>% as.character
            )
          }
        )
      )
    }

    # 3) Push all squad data
    sData <- teamData %>%
      purrr::map(function(x) x %>% `[[`('squad'))

    # Just make sure none are NULL
    sNull <- sData %>%
      purrr::map(is.null) %>%
      purrr::flatten_lgl()

    if (sNull %>% any) {
      sNull %<>% `!`()
      allKeys$squad %<>% `[`(sNull)
      sData %<>% purrr::compact()
    }

    # Now add layered player data
    if (sData %>% length %>% `>`(0)) {

      lapply(
        X = 1:(sData %>% length),
        FUN = function(x) {

          # Define all objects first
          singleDF <- sData[[x]] %>% lapply(as.character)
          playerIDs <- singleDF$id
          sqKeys <- paste0(allKeys$squad, ":", playerIDs)
          fieldNames <- singleDF %>% names

          # A) Check if player has been added for the first time
          newPlayers <- KEYS$RED$pipeline(
            .commands = lapply(
              X = playerIDs,
              FUN = function(z) "c_playerSetInfo" %>% KEYS$PIPE$SADD(z)
            )
          ) %>%
            purrr::flatten_int() %>%
            as.logical

          # B) If they are new then analyse them later
          if (newPlayers %>% any) {
            playerIDs %<>% `[`(newPlayers)
            KEYS$RED$pipeline(
              .commands = lapply(
                X = playerIDs,
                FUN = function(z) "analysePlayers" %>% KEYS$PIPE$LPUSH(z)
              )
            )
          }

          # C) Add all squad information data
          KEYS$RED$pipeline(
            .commands = lapply(
              X = 1:(sqKeys %>% length),
              FUN = function(y) {
                sqKeys[y] %>% KEYS$PIPE$HMSET(
                  field = fieldNames,
                  value = singleDF %>% purrr::map(y) %>% as.character
                )
              }
            )
          )
        }
      )
    }
  }
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


commentary_sub <- function(KEYS, matchID, teamID, teamStats, commentary) {

  # Insert commentary here
  paste0("cmt_commentary:", KEYS$COMP, ":", matchID, ":", teamID) %>%
    KEYS$RED$HMSET(
      field = teamStats %>% names,
      value = teamStats %>% as.character
    )

  # Check if player stats exist
  playerStats <- commentary$player %>%
    purrr::when(is.null(.) ~ data.frame(), ~ .)

  # If any player stats exists then analyse them
  pRow <- playerStats %>% nrow
  if (pRow > 0) {
    # column names
    hashNames <- playerStats %>% names
    keyNames <- paste0("cmp:", KEYS$COMP, ":", matchID, ":", playerStats$id)

    # Insert all player information
    KEYS$RED$pipeline(
      .commands = lapply(
        X = 1:(keyNames %>% length),
        FUN = function(x) {
          keyNames[x] %>% KEYS$PIPE$HMSET(
            field = hashNames,
            value = playerStats[x, ]
          )
        }
      )
    )
  }
}
