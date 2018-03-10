#' @title Calculate Data Set
#'
#' @description A function that takes current statistical data and combines
#'  a series of features into a data frame one at a time and returns a
#'  data set ready for analysis
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'   }
#'
#' @param matchData A dataframe containing all the match data.
#' @param logger A boolean value to indicate whether to print useful
#'  log information to screen.
#'
#' @return Returns a dataframe containing the column names of 'returnItems'
#'  plus a few other metrics.
#'
#' @export


calculate_data <- function(matchData, logger = FALSE) {

  # Only take these names
  allowedNames <- c(
    'shots_total', 'shots_ongoal', 'fouls', 'corners', 'possesiontime', 'yellowcards', 'saves'
  )

  # Infer the season
  KEYS$SEASON <- matchData$season %>% footballstats::prs_season()
  mDat <- data.frame(stringsAsFactors = FALSE)

  # Data rows
  rowData <- matchData %>% nrow
  res <- c()

  # Set up progress bar
  progressBar <- utils::txtProgressBar(
    min = 0,
    max = rowData,
    style = 3
  )

  for (i in 1:rowData) {

    # Try to reconnect with redis
    footballstats::redis_con()

    # Track progress
    utils::setTxtProgressBar(progressBar, i)

    # Take a single slice of match data at a time
    matchSlice <- matchData[i, ]
    matchID <- matchSlice$id %>% as.integer
    KEYS$COMP <- matchSlice$comp_id
    teamIDs <- c(matchSlice$localteam_id, matchSlice$visitorteam_id)

    # Get single match information
    singleMatchInfo <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchID) %>%
      rredis::redisHGetAll()

    # 0) datSlice contains the match ID from the start
    datSlice <- data.frame(
      matchID = matchID,
      stringsAsFactors = FALSE
    )

    if (logger) print(datSlice)

    # 1) Get commentary information (Initialise datSlice)
    datSlice %<>% cbind(
      footballstats::feat_commentaries(
        KEYS = KEYS,
        matchID = matchID,
        teamIDs = teamIDs,
        commentaryNames = allowedNames
      )
    )

    if (logger) print(datSlice)

    # 2) Get form information
    datSlice %<>% cbind(
      footballstats::feat_form(
        matchData = matchData,
        teamIDs = teamIDs,
        singleMatchInfo = singleMatchInfo
      )
    )

    if (logger) print(datSlice)

    # Finally, find out how convincing the match was for the home team
    conv <- matchSlice$ft_score %>%
      footballstats::prs_ftscore()
    convince <- conv[1] - conv[2]

    # From convince, find out result as W / L / D
    res %<>% c(if (convince > 0) 'W' else if (convince == 0) 'D' else 'L')

    # Get relative position
    datSlice %<>% cbind(
      footballstats::feat_position(
        KEYS = KEYS,
        matchID = matchID,
        teamIDs = teamIDs
      )
    )

    if (logger) print(datSlice)

    mDat %<>% rbind(datSlice)
  }

  # Results are collected as a vector, column bind it onto the full dataframe
  mDat$res <- res

  # Close the progress bar
  close(progressBar)

  mDat %>% return()
}

#' @title Form Feature
#'
#' @description A function that takes a matchData data frame and two
#'  teamID values stored in \code{teamIDs}, with a single match piece
#'  of information and calculates the form of the team in the run up
#'  to the match information passed. The functionality is looped for both
#'  teamIDs.
#'
#' @param matchData A data frame
#' @param teamIDs An integer vector that contains c(home_id, away_id).
#' @param singleMatchInfo A data frame with one row that contains
#'  important match information between two teams.
#'
#' @export


feat_form <- function(matchData, teamIDs, singleMatchInfo) {
  forms <- c()
  for (j in 1:2) {
    # Calculate team form
    formResults <- footballstats::team_form(
      matchData = matchData,
      teamID = teamIDs[j]
    )

    # Find out form relative to current date.
    forms %<>% c(
      footballstats::relative_form(
        matchInfo = singleMatchInfo,
        totalForm = formResults
      )
    )
  }

  # Calculate the difference in forms
  form <- if (forms %>% length %>% `!=`(2)) {
    list(NA, NA)
  } else {
    # NEED TO RETURN TWO HERE!!!
    forms %>% lapply(footballstats::form_to_int)
  }

  # Return the data frame with form as the only column
  return(
    data.frame(
      `form.h` = form[[1]],
      `form.a` = form[[2]],
      stringsAsFactors = FALSE
    )
  )
}

#' @title Commentary Feature
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{cmt:_commentary:{comp_id}:{match_id}:{team_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @export


feat_commentaries <- function(KEYS, matchID, teamIDs, commentaryNames) {

  cResults <- c()
  for (j in 1:2) {
    commentaryKey <- paste0('cmt_commentary:', KEYS$COMP, ':', matchID, ':', teamIDs[j]) %>%
      rredis::redisKeys() %>% as.character

    # Check commentary key exists
    if (identical(commentaryKey, character(0))) break

    # Check that all the allowed names is a subset of the commentary
    availableNames <- commentaryKey %>% rredis::redisHGetAll() %>% names
    if (commentaryNames %in% availableNames %>% all %>% `!`()) break

    commentary <- footballstats::commentary_from_redis(
      keyName = commentaryKey,
      returnItems = commentaryNames
    )

    # Get Commentary results from Redis
    cResults %<>% c(
      footballstats::commentary_from_redis(
        keyName = commentaryKey,
        returnItems = commentaryNames
      ) %>% list
    )
  }

  # Return a mini frame containing form information
  c(commentaryNames %>% paste0('.h'), commentaryNames %>% paste0('.a')) %>%
    footballstats::handle_projections(resList = cResults) %>%
    return()
}

#' @title Relative Position Feature
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[KEY]} :: \code{c_startDate:{comp_id}:{season}}}
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @export


feat_position <- function(KEYS, matchID, teamIDs, matchDate = NULL) {

  # Get the start date
  startDate <- paste0('c_startDate:', KEYS$COMP, ':', KEYS$SEASON) %>%
    rredis::redisGet() %>%
    as.integer

  # Get the current date
  currentDate <- if (matchDate %>% is.null) {
    paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchID) %>%
      rredis::redisHGet(field = 'formatted_date') %>%
      as.character %>%
      as.Date(format = '%d.%m.%Y') %>%
      as.integer
  } else {
    matchDate %>%
      as.Date(format = '%d.%m.%Y') %>%
      as.integer
  }

  # Convert to week number
  weekNum <- currentDate %>%
    `-`(startDate) %>%
    `/`(7) %>%
    floor %>%
    `+`(1)

  # Position key
  posKey <- paste0('cw_pl:', KEYS$COMP, ':', KEYS$SEASON, ':')

  # Get the last known position of the two teams
  weekKeys <- posKey %>%
    paste0('*') %>%
    rredis::redisKeys() %>%
    footballstats::get_weeks()

  # Get the positions from the week being investigated
  positions <- posKey %>%
    paste0(weekKeys %>% `[`(weekNum %>% `-`(weekKeys) %>% abs %>% which.min)) %>%
    rredis::redisHGetAll() %>%
    lapply(as.integer)

  # Determine & Return relative position as a data.frame
  data.frame(
    `position.h` = positions[[teamIDs[1]]],
    `position.a` = positions[[teamIDs[2]]],
    stringsAsFactors = FALSE
  ) %>% return()
}
