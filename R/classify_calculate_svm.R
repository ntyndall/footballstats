#' @title classify_calculate_svm
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


classify_calculate_svm <- function(competitionID, seasonStarting, commentaryKeys, 
                                   commentaryNames, matchData,
                                   totalData = data.frame(stringsAsFactors = FALSE)) {
  
  for (i in 1:length(commentaryKeys)) {
    # Parse out commentary items and create a single row data frame
    singleCommentary <- commentaryKeys[i]
    elementsSplit <- strsplit(singleCommentary, ':')[[1]]
    matchID <- elementsSplit[3]
    teamID <- elementsSplit[4]
    
    # Get Commentary results from Redis
    results <- classify_commentary_from_redis(keyName = singleCommentary,
                                              returnItems = commentaryNames)

    # Create single row of information
    singleItem <- data.frame(t(results), stringsAsFactors = FALSE)
    names(singleItem) <- commentaryNames
    
    # Get single match information
    matchInfo <- redisConnection$HGETALL(key = paste0('csm:', competitionID, ':', seasonStarting, ':', matchID))
    singleMatchInfo <- matchInfo[c(FALSE, TRUE)]
    names(singleMatchInfo) <- matchInfo[c(TRUE, FALSE)]

    # Need to choose which current team is being analysed for each match.
    currentTeam <- singleMatchInfo[as.integer(which(singleMatchInfo == teamID))]
    scoreCurrent <- as.integer(singleMatchInfo[currentTeam %>% purrr::when(. == 'localteam_id' ~ 'localteam_score', ~ 'visitorteam_score')])
    scoreOther <-  as.integer(singleMatchInfo[currentTeam %>% purrr::when(. == 'localteam_id' ~ 'visitorteam_score', ~ 'localteam_score')])
    
    # Get the result of the match whether it was a home or away game
    winLoseDraw <- classify_match_result(scoreCurrent = scoreCurrent, 
                                         scoreOther = scoreOther)

    # Calculate team form (Don't include if 3 matches don't exist yet!)
    formResults <- classify_team_form(matchData = matchData, 
                                      teamID = teamID)
    
    # Create a data frame of forms and dates.
    totalForm <- data.frame(date = formResults[[2]],
                            form = formResults[[1]],
                            stringsAsFactors = FALSE)
  
    # Find out form relative to current date.
    form <- classify_relative_form(matchInfo = singleMatchInfo,
                                   totalForm = totalForm)

    # Only if the match has seen 3 previous games do we add a row to the totalData frame
    # This keeps the forms consistent to the previous 3 matches!
    if (!is.null(form)) {
      # Append the form and results to single row data
      singleItem$form <- form
      singleItem$res <- winLoseDraw
    
      # Calculate additional metrics
      #singleItem <- calculateAdditionalMetrics(competitionID = competitionID,
      #                                         teamID = teamID,
      #                                         seasonStarting = seasonStarting,
      #                                         singleItem = singleItem)
  
      # Bind the data frames together into one
      totalData <- rbind(totalData, singleItem)
    }
  }
  return(totalData)
}
