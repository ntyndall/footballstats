#' @title calculate_svm
#'
#' @description A function that takes current statistical data and combines
#'  it into a dataframe to be passed later to an SVM classifier.
#'
#' @param competitionID An integer value denoting the competition ID.
#' @param seasonStarting An integer denoting the start year of the season.
#' @param commentaryKeys A vector of character values that hold the
#'  commentary statistic hash maps in redis.
#' @param matchData A dataframe containing all the match data.
#' @param totalData A null dataframe which initialises the return value.
#'
#' @return Returns a dataframe containing the column names of 'returnItems'
#'  plus a few other metrics.
#'
#' @export


calculate_svm <- function(matchData) {

  # Only take these names
  allowedNames <- dataScales$sMax %>% names %>% `[`(c(1:7))

  # Infer competitionID
  #competitionID <- matchData$comp_id %>% footballstats::prs_comp()
  seasonStarting <- matchData$season %>% footballstats::prs_season()
  mDat <- data.frame(stringsAsFactors = FALSE)

  for (i in 1:(matchData %>% nrow)) {
    matchSlice <- matchData[i, ]
    matchID <- matchSlice$id
    competitionID <- matchSlice$comp_id

    # Get single match information
    singleMatchInfo <- rredis::redisHGetAll(
      key = paste0('csm:', competitionID, ':', seasonStarting, ':', matchID))
    teamIDs <- c(matchSlice$localteam_id, matchSlice$visitorteam_id)

    # Get information
    datSlice <- footballstats::build_model(
      competitionID = competitionID,
      matchID = matchID,
      teamIDs = teamIDs,
      commentaryNames = allowedNames,
      matchData = matchData,
      singleMatchInfo = singleMatchInfo)

    mDat %<>% rbind(datSlice)
  }
  # Replace any NA's with zero
  mDat[mDat %>% is.na] <- 0

  mDat %>% return()
}

#'
#' @export


build_model <- function(competitionID, matchID, teamIDs, commentaryNames, matchData, singleMatchInfo) {

  returnData <- data.frame(stringsAsFactors = FALSE)
  for (j in 1:2) {
    commentaryKey <- paste0('cmt_commentary:', competitionID, ':', matchID, ':', teamIDs[j]) %>%
      rredis::redisKeys() %>% as.character

    # Check commentary key exists
    if (identical(commentaryKey, character(0))) break

    # Check that all the allowed names is a subset of the comentary
    availableNames <- commentaryKey %>% rredis::redisHGetAll() %>% names
    if (allowedNames %in% availableNames %>% all %>% `!`()) break

    # Get Commentary results from Redis
    results <- footballstats::commentary_from_redis(
      keyName = commentaryKey,
      returnItems = commentaryNames)

    # Calculate team form
    formResults <- footballstats::team_form(
      matchData = matchData,
      teamID = teamIDs[j])

    # Create a data frame of forms and dates.
    totalForm <- data.frame(
      date = formResults[[2]],
      form = formResults[[1]],
      stringsAsFactors = FALSE)

    # Find out form relative to current date.
    form <- footballstats::relative_form(
      matchInfo = singleMatchInfo,
      totalForm = totalForm)

    if (is.null(form)) {
      break
    } else {
      if (j == 1) {
        fRes <- results
        fFirst <- form %>% footballstats::form_to_int()
      } else {
        winLoseDraw <- footballstats::match_result(
          scoreCurrent = singleMatchInfo$localteam_score %>% as.integer,
          scoreOther = singleMatchInfo$visitorteam_score %>% as.integer)

        # Create single row of information
        singleItem <- data.frame(t(fRes - results), stringsAsFactors = FALSE)
        names(singleItem) <- commentaryNames

        # Append form and result to data frame
        singleItem$form <- `-`(fFirst, form %>% footballstats::form_to_int())
        singleItem$res <- winLoseDraw

        # Bind the data frames together into one
        returnData <- singleItem
      }
    }
  }

  returnData %>% return()
}
