#' @title Calculate Data Set
#'
#' @description A function that takes current statistical data and combines
#'  it into a dataframe to be passed later to an SVM classifier.
#'
#' @param matchData A dataframe containing all the match data.
#'
#' @return Returns a dataframe containing the column names of 'returnItems'
#'  plus a few other metrics.
#'
#' @export


calculate_data <- function(matchData, logger = FALSE) {

  # Only take these names
  allowedNames <- dataScales$sMax %>% names %>% `[`(c(1:7))

  # Infer the season
  seasonStarting <- matchData$season %>% footballstats::prs_season()
  mDat <- data.frame(stringsAsFactors = FALSE)

  # Data rows
  rowData <- matchData %>% nrow
  res <- c()

  # Set up progress bar
  progressBar <- utils::txtProgressBar(
    min = 0,
    max = rowData,
    style = 3)

  for (i in 1:rowData) {

    # Track progress
    utils::setTxtProgressBar(progressBar, i)

    # Take a single slice of match data at a time
    matchSlice <- matchData[i, ]
    matchID <- matchSlice$id %>% as.integer
    competitionID <- matchSlice$comp_id
    teamIDs <- c(matchSlice$localteam_id, matchSlice$visitorteam_id)

    # Get single match information
    singleMatchInfo <- rredis::redisHGetAll(
      key = paste0('csm:', competitionID, ':', seasonStarting, ':', matchID))

    # 0) datSlice contains the match ID from the start
    datSlice <- data.frame(
      matchID = matchID,
      stringsAsFactors = FALSE
    )

    if (logger) print(datSlice)

    # 1) Get commentary information (Initialise datSlice)
    datSlice %<>% cbind(
      footballstats::feat_commentaries(
        competitionID = competitionID,
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
    datSlice$convince <- convince

    # From convince, find out result as W / L / D
    res %<>% c(if (convince > 0) 'W' else if (convince == 0) 'D' else 'L')

    # Get relative position
    datSlice %<>% cbind(
      footballstats::feat_position(
        competitionID = competitionID,
        seasonStarting = seasonStarting,
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
  # Replace any NA's with zero
  # (NOT YET)
  #mDat[mDat %>% is.na] <- 0

  mDat %>% return()
}

#' @title Relative Form Feature
#' @export


feat_form <- function(matchData, teamIDs, singleMatchInfo) {
  forms <- c()
  for (j in 1:2) {
    # Calculate team form
    formResults <- footballstats::team_form(
      matchData = matchData,
      teamID = teamIDs[j]
    )

    # Create a data frame of forms and dates.
    totalForm <- data.frame(
      date = formResults[[2]],
      form = formResults[[1]],
      stringsAsFactors = FALSE
    )

    # Find out form relative to current date.
    forms %<>% c(
      footballstats::relative_form(
        matchInfo = singleMatchInfo,
        totalForm = totalForm
      )
    )
  }

  # Calculate the difference in forms
  form <- if (forms %>% length %>% `!=`(2)) {
    NA
  } else {
    differ <- footballstats::form_to_int(oldForms = forms)
    differ[1] - differ[2]
  }

  # Return the data frame with form as the only column
  return(
    data.frame(
      form = form,
      stringsAsFactors = FALSE
    )
  )
}

#' @title Commentary Feature
#' @export


feat_commentaries <- function(competitionID, matchID, teamIDs, commentaryNames) {

  cResults <- c()
  for (j in 1:2) {
    commentaryKey <- paste0('cmt_commentary:', competitionID, ':', matchID, ':', teamIDs[j]) %>%
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

  commentary <- if (cResults %>% length %>% `!=`(2)) {
    NA %>% rep(commentaryNames %>% length) %>% t
  } else {
    cResults[[1]] %>% `-`(cResults[[2]]) %>% t
  }

  # Return the data frame with form as the only column
  dF <- commentary %>% data.frame(stringsAsFactors = FALSE)
  names(dF) <- commentaryNames
  dF %>% return()

}

#' @title Relative Position Feature
#' @export


feat_position <- function(competitionID, seasonStarting, matchID, teamIDs) {

  # Get the start date
  startDate <- paste0('c_startDate:', competitionID, ':', seasonStarting) %>%
    rredis::redisGet() %>%
    as.integer

  # Get the current date
  currentDate <- paste0('csm:', competitionID, ':', seasonStarting, ':', matchID) %>%
    rredis::redisHGet(field = 'formatted_date') %>%
    as.character %>%
    as.Date(format = '%d.%m.%Y') %>%
    as.integer

  # Convert to week number
  weekNum <- currentDate %>%
    `-`(startDate) %>%
    `/`(7) %>%
    floor %>%
    `+`(1)

  # Get the positions from the week being investigated
  positions <- paste0('cw_pl:', competitionID, ':', seasonStarting, ':', weekNum) %>%
    rredis::redisHGetAll() %>%
    lapply(as.integer)

  # Determine & Return relative position as a data.frame
  data.frame(
    relativePos = positions[[teamIDs[1]]] - positions[[teamIDs[2]]],
    stringsAsFactors = FALSE
  ) %>% return()

}

