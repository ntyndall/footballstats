#' @title Calculate SVM Data
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


calculateSVMData <- function(competitionID, seasonStarting, commentaryKeys, 
                             commentaryNames, matchData,
                             totalData = data.frame(stringsAsFactors = FALSE)) {
  
  for (i in 1:length(commentaryKeys)) {
    # Parse out commentary items and create a single row data frame
    singleCommentary <- commentaryKeys[i]
    elementsSplit <- strsplit(singleCommentary, ':')[[1]]
    matchID <- elementsSplit[3]
    teamID <- elementsSplit[4]
    
    # Get Commentary results from Redis
    results <- getCommentaryFromRedis(keyName = singleCommentary,
                                      returnItems = commentaryNames)

    # Create single row of information
    singleItem <- data.frame(t(results), stringsAsFactors = FALSE)
    names(singleItem) <- commentaryNames
    
    # Get single match information
    matchInfo <- redisConnection$HGETALL(key = paste0('csm:', competitionID, ':', seasonStarting, ':', matchID))
    singleMatchInfo <- matchInfo[c(FALSE, TRUE)]
    names(singleMatchInfo) <- matchInfo[c(TRUE, FALSE)]

    # Get the result of the match whether it was a home or away game
    winLoseDraw <- resultOfMatch(scoreHome = as.integer(singleMatchInfo$localteam_score), 
                                 scoreAway = as.integer(singleMatchInfo$visitorteam_score), 
                                 homeOrAway = names(which(singleMatchInfo == teamID)))

    # Calculate team form (Don't include if 3 matches don't exist yet!)
    formResults <- calculateTeamForm(matchData = matchData, 
                                     teamID = teamID, 
                                     whichTeam = c('localteam_id', 'visitorteam_id'))
    
    # Create a data frame of forms and dates.
    totalForm <- data.frame(date = c(formResults[[1]][[2]], formResults[[2]][[2]]),
                            form = c(formResults[[1]][[1]], formResults[[2]][[1]]),
                            stringsAsFactors = FALSE)
    
    # Find out form relative to current date.
    form <- formRelativeToDate(matchInfo = singleMatchInfo,
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
