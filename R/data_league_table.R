#' @title Create weekly points
#'
#' @description A function that takes a data frame of matchData and recreates
#'  the league table by adding points on a weekly basis based on the date
#'  timestamps of the data frame.
#'
#'  Make sure the match data has been recreated, i.e. it needs to be stored
#'  in redis, then ordered by date to work.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[KEY]} :: \code{c_startDate:{comp_id}:{season}}}
#'     \item{\strong{[SET]} :: \code{leagueMatchSet}}
#'     \item{\strong{[HASH]} :: \code{cwt_l:{comp_id}:{season}:{week#}:{team_id}}}
#'   }
#'
#' @param matchData A data frame that contains rows of single matches
#'  that have been played between two teams.
#'
#' @return Nothing. The points and goal information is stored in redis.
#'
#' @export


create_table <- function(KEYS, matchData) {

  # Get the date key
  dateKey <- paste0('c_startDate:', KEYS$COMP, ':', KEYS$SEASON)

  # Get the start date
  startDate <- dateKey %>%
    KEYS$RED$GET()

  # If it doesn't exist then create it
  if (startDate %>% is.null) {
    startDate <- matchData$zzz.date %>% as.integer %>% min
    dateKey %>%
      KEYS$RED$SET(
        value = startDate
      )
  } else {
    startDate %<>% as.integer
  }

  matchList <- matchData %>% as.list
  matchList[c("zzz.date", "home.score", "away.score")] %<>% lapply(as.integer)

  # Get all unique teamIDs
  uniqueTeams <- c(matchList$home.id, matchList$away.id) %>%
    unique

  uniqueNames <- c(matchList$home.team, matchList$away.team) %>%
    unique

  # Get all the previous keys
  prevKeys <- paste0('cwt_l:', KEYS$COMP, ':', KEYS$SEASON, ':*') %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr()

  # Get previous week for EVERY
  lastData <- KEYS %>% footballstats::get_last_week(uniqueTeams = uniqueTeams)

  scores <- c(matchList$home.score, matchList$away.score)

  # Get points from result
  res <- matchList$home.score - matchList$away.score

  # Stack useful vectorc
  allInfo <- list(
    ids = matchList$zzz.matchID %>% rep(2),
    tids = c(matchList$home.id, matchList$away.id),
    teams = c(matchList$home.team, matchList$away.name),
    pts = c(
      sapply(res, FUN = function(x) if (x > 0) 3 else if (x < 0) 0 else 1),
      sapply(res, FUN = function(x) if (x > 0) 0 else if (x < 0) 3 else 1)
    ),
    gf = c(matchList$home.score, matchList$away.score),
    ga = c(matchList$away.score, matchList$home.score),
    week = matchList$zzz.date %>% `-`(startDate) %>% `/`(7) %>% floor %>% `+`(1) %>% rep(2)
  )

  # Define set key
  sKey <- paste0("leagueMatchSet:", KEYS$SEASON)

  # Now get rid of indexes that have actually been investigated
  alreadyAdded <- KEYS$RED$pipeline(
    .commands = lapply(
      X = paste0(allInfo$ids, ":", allInfo$tids),
      FUN = function(x) sKey %>% KEYS$PIPE$SADD(x)
    )
  ) %>%
    purrr::flatten_int() %>%
    as.logical %>%
    `!`()

  # Filter out data if it has already been added before
  if (alreadyAdded %>% any) {
    if (alreadyAdded %>% all) {
      allInfo <- NULL
    } else {
      allInfo %<>% purrr::map(function(x) x %>% `[`(alreadyAdded %>% `!`()))
    }
  }

  # If data is to be added, loop over unique teamIDs
  if (allInfo %>% is.null %>% `!`()) {
    newUniques <- allInfo$tids %>% unique
    allInfo$allKeys <- paste0('cwt_l:', KEYS$COMP, ':', KEYS$SEASON, ':', allInfo$week, ':', allInfo$tids)

    # Lapply over every uniqueTeam now and create new (filter if they have been analysed already)
    myresults <- lapply(
      X = 1:(newUniques %>% length),
      FUN = function(x) {

        # Only look at one team at a time
        singleTeam <- allInfo %>%
          purrr::map(function(z) z %>% `[`(newUniques[x] %>% `==`(allInfo$tids)))

        # Order by week so I can accumulate easily
        singleTeam %<>%
          purrr::map(function(x) x %>% `[`(singleTeam$week %>% order))

        # Get team name
        tName <- singleTeam$teams %>% unique %>% `[`(1)

        # Get cumulative sum
        csum <- function(x) x %>% cumsum %>% `[`(-1)

        # Character teamid
        charInd <- newUniques[x] %>% as.character

        # Create
        cPts <- c(lastData$table[[charInd]]$PTS %>% as.integer, singleTeam$pts) %>% csum() %>% as.character
        cGf <- c(lastData$table[[charInd]]$GF %>% as.integer, singleTeam$gf) %>% csum() %>% as.character
        cGd <- c(lastData$table[[charInd]]$GD %>% as.integer, (singleTeam$gf - singleTeam$ga)) %>% csum() %>% as.character

        # Start to create new keys
        blnk <- KEYS$RED$pipeline(
          .commands = lapply(
            X = 1:(singleTeam$allKeys %>% length),
            FUN = function(y) {
              singleTeam$allKeys[y] %>% KEYS$PIPE$HMSET(
                field = c("TEAM", "PTS", "GF", "GD"),
                value = c(tName, cPts[y], cGf[y], cGd[y])
              )
            }
          )
        )
      }
    )
  }
  return(NULL)
}

#' @title Weekly Positions
#'
#' @description A function that can take the data stored by \code{create_table} and
#'  reproduce a list of standings for every team based on each week.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{cwt_l:{comp_id}:{season}:{week#}:{team_id}}}
#'     \item{\strong{[HASH]} :: \code{cw_pl:{comp_id}:{season}:{week#}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @return Nothing. The weekly position of each team is stored in redis.
#'
#' @export


weekly_positions <- function(KEYS) {

  # Get all possible keys
  redisKeys <- paste0('cwt_l:', KEYS$COMP, ':', KEYS$SEASON, '*') %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr()

  # Get all unique teamIDs as a reference
  allTeams <- redisKeys %>%
    strsplit(split = ':') %>%
    purrr::map(5) %>%
    purrr::flatten_chr() %>%
    unique

  # Get the unique weeks to loop over
  weeks <- redisKeys %>% footballstats::get_weeks()

  # Determine unique weeks and other boundaries
  uniqKeys <- weeks %>% unique %>% sort
  uniqLen <- uniqKeys %>% length
  uniqMin <- uniqKeys %>% min

  # Loop over all the unique keys..
  for (i in 1:uniqLen) {

    # Get subkeys
    subKeys <- redisKeys %>%
      subset(paste0(KEYS$SEASON, ':', uniqKeys[i], ':') %>% grepl(redisKeys))

    # There could be non-zero weeks..
    if (subKeys %>% identical(character(0))) next

    # If the keys already exist
    if (paste0('cw_pl:', KEYS$COMP, ':', KEYS$SEASON, ':', uniqKeys[i]) %>% KEYS$RED$EXISTS() %>% as.logical) next

    # Get the teamIDs
    teamIDs <- subKeys %>%
      strsplit(split = ':') %>%
      purrr::map(5) %>%
      purrr::flatten_chr()

    # Set an empty points list
    singleWeek <- data.frame(stringsAsFactors = FALSE)

    # Are there any teams missing??
    allPlayed <- allTeams %in% teamIDs
    if (allPlayed %>% all %>% `!`()) {
      lookPrevious <- allTeams %>% subset(allPlayed %>% `!`())
      for (j in 1:(lookPrevious %>% length)) {
        prevWeeks <- paste0('cwt_l:', KEYS$COMP, ':', KEYS$SEASON, ':*:', lookPrevious[j]) %>%
          KEYS$RED$KEYS() %>%
          purrr::flatten_chr()

        oneTeamWeeks <- prevWeeks %>% footballstats::get_weeks()
        prevWeek <- prevWeeks %>%
          `[`(
            uniqKeys[i] %>%
              `-`(oneTeamWeeks) %>%
              abs %>%
              which.min
          )
        subFrame <- prevWeek %>%
          KEYS$RED$HGETALL() %>%
          footballstats::create_hash()

        subFrame$TEAM <- NULL
        subFrame %<>%
          lapply(as.integer) %>%
          data.frame(stringsAsFactors = FALSE)
        subFrame$teamID <- lookPrevious[j]
        singleWeek %<>% rbind(subFrame)
      }
    }

    subKeyLen <- subKeys %>% length

    for (j in 1:subKeyLen) {
      # At this point I need to order them by something!
      subFrame <- subKeys[j] %>%
        KEYS$RED$HGETALL() %>%
        footballstats::create_hash()

      subFrame$TEAM <- NULL
      subFrame %<>%
        lapply(as.integer) %>%
        data.frame(stringsAsFactors = FALSE)

      subFrame$teamID <- teamIDs[j]
      # build the dataframe
      singleWeek %<>% rbind(subFrame)
    }
    # order frame by points.. gd.. then gf...
    singleWeek <- singleWeek[
      order(
        singleWeek$PTS,
        singleWeek$GD,
        singleWeek$GF,
        decreasing = TRUE
      ), ]

    # Push list of positions to the cw_pl hashmap ...
    paste0('cw_pl:', KEYS$COMP, ':', KEYS$SEASON, ':', uniqKeys[i]) %>%
      KEYS$RED$HMSET(
        field = singleWeek$teamID,
        value = c(1:(singleWeek %>% nrow)) %>% as.character
      )
  }
}

#' @title Get Weeks
#'
#' @description A function that is used by the data league table functionality
#'  in order to extract the week information from the keys
#'
#' @param x A character vector of \code{cwt_l} style keys
#'
#' @return An integer vector of weeks that are defined in the redis keys.
#'
#' @export


get_weeks <- function(x) {
  x %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    return()
}
