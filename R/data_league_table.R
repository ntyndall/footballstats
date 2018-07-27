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


create_table <- function(matchData) {

  # Get the competitionID first
  competitionID <- matchData$comp_id %>%
    unique

  # Get the unique season (should probably get this from KEYS!!!)
  seasonStarting <- matchData$season %>%
    unique %>%
    strsplit(split = '/') %>%
    purrr::map(1) %>%
    purrr::flatten_chr()

  # SET the very first date if it doesn't exist!
  firstDate <- paste0('c_startDate:', competitionID, ':', seasonStarting)
  if (firstDate %>% rredis::redisExists() %>% `!`()) {
    firstElement <- matchData[1, ]$formatted_date %>%
      as.integer %>%
      as.character %>%
      charToRaw()
    firstDate %>% rredis::redisSet(value = firstElement)
  }

  # GET the starting date
  startDate <- firstDate %>%
    rredis::redisGet() %>%
    as.integer

  # Loop over all match data
  for (i in 1:(matchData %>% nrow)) {

    # Break match data down to individual slices
    slice <- matchData[i, ]
    matchID <- slice$id
    currentDate <- slice$formatted_date %>% as.integer
    teamIDs <- c(slice$localteam_id, slice$visitorteam_id)
    teamNames <- c(slice$localteam_name, slice$visitorteam_name)
    teamScore <- slice$ft_score %>% footballstats::prs_ftscore()

    if (teamScore %>% is.na %>% any) next
    scoreDiff <- teamScore[1] - teamScore[2]

    # Calculate the `on the fly` week number based on date away from first game!
    weekNum <- currentDate %>% `-`(startDate) %>% `/`(7) %>% floor %>% `+`(1)

    if (scoreDiff > 0) {
      hPts <- 3; aPts <- 0
    } else if (scoreDiff == 0) {
      hPts <- aPts <- 1
    } else {
      hPts <- 0; aPts <- 3
    }

    for (j in 1:2) {
      redisKey <- paste0('cwt_l:', competitionID, ':', seasonStarting, ':', weekNum, ':', teamIDs[j])

      pointsToAdd <- 'leagueMatchSet' %>% rredis::redisSAdd(
         element = paste0(matchID, ':', teamIDs[j]) %>% charToRaw()
        ) %>% as.integer %>% as.logical

      # Create the key if it doesnt exist
      if (pointsToAdd) {

        prevKey <- paste0('cwt_l:', competitionID, ':', seasonStarting, ':*:', teamIDs[j]) %>%
          rredis::redisKeys()

        if (weekNum %>% `!=`(1) && prevKey %>% is.null %>% `!`()) {
          # Get last match info
          oneTeamWeeks <- prevKey %>% footballstats::get_weeks()
          prevWeek <- prevKey %>%
            `[`(weekNum %>%
                  `-`(oneTeamWeeks) %>%
                  abs %>% which.min
            )

          pRes <- prevWeek %>%
            rredis::redisHGetAll()
          pRes$TEAM <- NULL
          pRes %<>% lapply(as.integer)
        } else {
          pRes <- list(PTS = 0, GF = 0, GD = 0)
        }

        # j = 1 for home, else away team stats
        toAdd <- if (j == 1) {
          list(pts = hPts, score = teamScore[1], diff = scoreDiff)
        } else {
          list(pts = aPts, score = teamScore[2], diff = -scoreDiff)
        }

        # Add the results to the hash
        redisKey %>% rredis::redisHMSet(
          values = list(
            TEAM = teamNames[j],
            PTS = pRes$PTS %>% `+`(toAdd$pts) %>% as.character %>% charToRaw(),
            GF = pRes$GF %>% `+`(toAdd$score) %>% as.character %>% charToRaw(),
            GD = pRes$GD %>% `+`(toAdd$diff) %>% as.character %>% charToRaw()
          )
        )
      }
    }
  }
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
    rredis::redisKeys()

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
      subset(paste0(':', uniqKeys[i], ':') %>% grepl(redisKeys))

    # There could be non-zero weeks..
    if (subKeys %>% identical(character(0))) next

    # If the keys already exist
    if (paste0('cw_pl:', KEYS$COMP, ':', KEYS$SEASON, ':', uniqKeys[i]) %>% rredis::redisExists()) next

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
          rredis::redisKeys()
        oneTeamWeeks <- prevWeeks %>% footballstats::get_weeks()
        prevWeek <- prevWeeks %>%
          `[`(uniqKeys[i] %>%
                `-`(oneTeamWeeks) %>%
                abs %>% which.min
            )
        subFrame <- prevWeek %>%
          rredis::redisHGetAll()

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
        rredis::redisHGetAll()

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

    position <- c(1:(singleWeek %>% nrow)) %>%
      as.character %>%
      sapply(charToRaw)
    names(position) <- singleWeek$teamID

    # Push list of positions to the cw_pl hashmap ...
    paste0('cw_pl:', KEYS$COMP, ':', KEYS$SEASON, ':', uniqKeys[i]) %>%
    rredis::redisHMSet(
      values = position
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
