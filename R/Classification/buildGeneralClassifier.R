#' @title Optimize SVM
#'
#' @description A function that loops through all possibilities of the variables
#'  to be included in the classifier. That means the recent form and results 
#'  will be better represented as the best classifier will be built on the most
#'  recent results.
#'
#' @details 1) recreateMatchData():
#'   2) calculateSVMData() -> resultOfMatch() -> { calculateTeamForm() -> resultOfMatch() }
#'      -> calculateAdditionalMetrics():
#'   3) getBinns():
#'   4) optimizeSVM() -> calculateBestSVMFit() 
#'      -> { generatePredictions() -> 
#'                  lookUpSlackEmoji() -> 
#'                  teamAbbreviations() ->
#'                  { getHomeAndAwayStats() -> commentaryStatistics() } -> 
#'                  { getFormFromMatchIDs() -> resultOfMatch() } ->
#'                  mapFormToInteger() }
#'   5) predictFutureMatches() -> formatDates() -> getGeneralData() ->  { generatePredictions() -> ... }
#'
#' @param redisConnection
#' @param competitionID
#' @param seasonStarting
#' @param returnItems
#' @param matchLimit
#'
#' @return A list containing the best SVM calculated in the first key, and 
#'  the best factors to use in the second key.



buildGeneralClassifier <- function(redisConnection, competitionID, seasonStarting,
                                   returnItems, matchLimit = 150) {

  # Query Redis and return everything from the competition. 
  print(paste0(Sys.time(), ' : Recreating match data.'))
  matchData <- recreateMatchData(redisConnection = redisConnection, 
                                 competitionID = competitionID, 
                                 seasonStarting = seasonStarting,
                                 matchLimit = matchLimit)

  # Check the keyNames from the current list of commentarys.
  commentaryKeys <- as.character(redisConnection$KEYS(pattern = paste0('cmt_commentary:', competitionID, '*')))
  commentaryNames <- checkAvailableCommentaryNames(commentaryKeys = commentaryKeys)

  # Construct data set for building an SVM
  print(paste0(Sys.time(), ' : Creating a dataframe from the match data.'))
  totalData <- calculateSVMData(competitionID = competitionID,
                                seasonStarting = seasonStarting,
                                commentaryKeys = commentaryKeys,
                                commentaryNames = commentaryNames,
                                matchData = matchData)

  # Get the binning limits
  binList <- getBinIntervals(totalData = totalData)
  
  # Map current form to an integer value also.
  totalData$form <- mapFormToInteger(oldForms = totalData$form)
  
  # Map the values from the binList to a number between... -(binNo) <= x <= -1
  totalData <- findBinIntervals(dataSet = totalData,
                                binList = binList)

  # Test the last match data...
  testData <- matchData[(nrow(matchData) - 9):nrow(matchData), ]
  totalData <- totalData[-c((nrow(totalData) - 18):nrow(totalData)), ]
  
  # Optimize the SVM by looping through all available variables
  matchFieldNames <- c('formatted_date', 'localteam_score', 'localteam_id', 'visitorteam_score', 'visitorteam_id')
  print(paste0(Sys.time(), ' : Optimizing the SVM Classifier.'))
  SVMDetails <- optimizeSVM(totalData = totalData,
                            seasonStarting = seasonStarting,
                            testData = testData,
                            binList = binList,
                            returnItems = commentaryNames,
                            matchFieldNames = matchFieldNames,
                            testing = TRUE)
 
  # Predict actual future results
  print(paste0(Sys.time(), ' : Predicting actual upcoming fixtures.'))
  predictFutureMatches(competitionID = competitionID,
                       seasonStarting = seasonStarting,
                       returnItems = returnItems,
                       subsetItems = SVMDetails[[2]],
                       SVMfit = SVMDetails[[1]])
}
