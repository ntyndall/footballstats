



buildGeneralClassifier <- function(redisConnection, competitionID, matchData, seasonStarting,
                                   matchLimit = 75) {

  # Query Redis and return everything from the competition. 
  matchData <- recreateMatchData(redisConnection = redisConnection, 
                                 competitionID = competitionID, 
                                 seasonStarting = seasonStarting)
  
  # Only look back at the previous `x` matches.
  if (nrow(matchData) == 0) {
    return(paste0(Sys.time(), ' : No match data found for the providing input parameters.'))
  } else if (nrow(matchData) > matchLimit) {
    matchData <- matchData[1:matchLimit, ]
  }

  # Construct data set for building an SVM
  totalData <- calculateSVMData(competitionID = competitionID,
                                seasonStarting = seasonStarting,
                                commentaryKeys = as.character(redisConnection$KEYS(pattern = 'cmt_commentary*')),
                                matchData = matchData,
                                returnItems = c('shots_total', 'shots_ongoal', 'fouls', 'corners', 'possesiontime', 'yellowcards', 'saves'))
  
  # Build and tune an SVM
  SVMFit <- calculateBestSVMFit(totalData = totalData)

  # Predict future results
  resultsPredicted <- predictFutureMatches(competitionID = competitionID,
                                           seasonStarting = seasonStarting,
                                           returnItems = c('shots_total', 'shots_ongoal', 'fouls', 'corners', 'possesiontime', 'yellowcards', 'saves'),
                                           SVMfit = SVMFit)
  
  # Send data to Slack
  sendResultsToSlack(resultsPredicted = resultsPredicted)
  
}


