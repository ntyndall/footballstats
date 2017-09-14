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
#' @param returnItems A vector of character values that hold the names of
#'  fields to be returned for the commentary statistics.
#' @param totalData A null dataframe which initialises the return value.
#'
#' @return Returns a dataframe containing the column names of 'returnItems'
#'  plus a few other metrics.
#'


calculateSVMData <- function(competitionID, seasonStarting, commentaryKeys, matchData,
                             returnItems, totalData = data.frame(stringsAsFactors = FALSE)) {

  for (i in 1:length(commentaryKeys)) {
    # Parse out commentary items and create a single row data frame
    singleCommentary <- commentaryKeys[i]
    elementsSplit <- strsplit(singleCommentary, ':')[[1]]
    teamID <- elementsSplit[4]
    results <- redisConnection$HMGET(key = singleCommentary,
                                     field = returnItems)
    names(results) <- returnItems
    results$possesiontime <- gsub(pattern = "%", 
                                  replacement = "", 
                                  x = results$possesiontime)
    singleItem <- data.frame(t(as.double(results)), 
                             stringsAsFactors = FALSE)
    names(singleItem) <- returnItems
    
    # Get single match information
    matchInfo <- redisConnection$HGETALL(key = paste0('csm:', competitionID, ':', seasonStarting, '/2018:', elementsSplit[3]))
    singleMatchInfo <- matchInfo[c(FALSE, TRUE)]
    names(singleMatchInfo) <- matchInfo[c(TRUE, FALSE)]

    # Get the result of the match whether it was a home or away game
    winLoseDraw <- resultOfMatch(scoreHome =  as.integer(singleMatchInfo$localteam_score), 
                                 scoreAway = as.integer(singleMatchInfo$visitorteam_score), 
                                 homeOrAway = names(which(singleMatchInfo == teamID)))

    # Calculate team form
    formResults <- calculateTeamForm(matchData = matchData, 
                                     teamID = teamID, 
                                     whichTeam = c('localteam_id', 'visitorteam_id'))
    
    totalForm <- data.frame(date = c(formResults[[1]][[2]], formResults[[2]][[2]]),
                            form = c(formResults[[1]][[1]], formResults[[2]][[1]]),
                            stringsAsFactors = FALSE)
    totalForm <- totalForm[sort.int(totalForm$date, decreasing = TRUE, index.return = TRUE)$ix, ]
    form <- paste(totalForm$form[1:3], collapse = '')

    # Append the form and results to single row data
    singleItem$form <- form
    singleItem$res <- winLoseDraw
  
    # Bind the data frames together into one
    totalData <- rbind(totalData, singleItem)
  }
  return(totalData)
}
