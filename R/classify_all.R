#' @title classify_all
#'
#' @description A function that loops through all possibilities of the variables
#'  to be included in the classifier. That means the recent form and results 
#'  will be better represented as the best classifier will be built on the most
#'  recent results.
#'
#' @details 1) classify_recreate_matchdata():
#'   2) calculateSVMData() -> classify_match_result() -> { classify_team_form() -> classify_match_result() }
#'      -> calculateAdditionalMetrics():
#'   3) getBinns():
#'   4) optimizeSVM() -> calculateBestSVMFit() 
#'      -> { classify_generate_predictions() -> 
#'                  classify_emoji() -> 
#'                  { classify_homeaway_stats() -> classify_commentary_stats() } -> 
#'                  { classify_form_from_match() -> classify_match_result() } ->
#'                  mapFormToInteger() }
#'   5) predictFutureMatches() -> formatDates() -> getGeneralData() ->  { classify_generate_predictions() -> ... }
#'
#' @param redisConnection
#' @param competitionID
#' @param seasonStarting
#' @param returnItems
#' @param matchLimit
#'
#' @return A list containing the best SVM calculated in the first key, and 
#'  the best factors to use in the second key.



classify_all <- function(redisConnection, competitionID, competitionName, 
                         seasonStarting, returnItems, matchLimit = 150) {

  # Query Redis and return everything from the competition. 
  print(paste0(Sys.time(), ' : Recreating match data.'))
  matchData <- classify_recreate_matchdata(redisConnection = redisConnection, 
                                           competitionID = competitionID, 
                                           seasonStarting = seasonStarting,
                                           matchLimit = matchLimit)

  matchData[ , c('localteam_id', 'localteam_name')]
  # Check the keyNames from the current list of commentarys.
  commentaryKeys <- as.character(redisConnection$KEYS(pattern = paste0('cmt_commentary:', competitionID, '*')))
  commentaryNames <- classify_available_commentaries(commentaryKeys = commentaryKeys)

  # Construct data set for building an SVM
  print(paste0(Sys.time(), ' : Creating a dataframe from the match data.'))
  totalData <- classify_calculate_svm(competitionID = competitionID,
                                      seasonStarting = seasonStarting,
                                      commentaryKeys = commentaryKeys,
                                      commentaryNames = commentaryNames,
                                      matchData = matchData)

  # Get the binning limits
  binList <- classify_get_bins(totalData = totalData)
  
  # Map current form to an integer value also.
  totalData$form <- classify_form_to_int(oldForms = totalData$form)
  
  # Map the values from the binList to a number between... -(binNo) <= x <= -1
  totalData <- classify_bin_intervals(dataSet = totalData,
                                      binList = binList)

  # Test the last match data...
  testData <- matchData[(nrow(matchData) - 9):nrow(matchData), ]
  totalData <- totalData[-c((nrow(totalData) - 18):nrow(totalData)), ]
  
  # Optimize the SVM by looping through all available variables
  matchFieldNames <- c('formatted_date', 'localteam_score', 'localteam_id', 'visitorteam_score', 'visitorteam_id')
  print(paste0(Sys.time(), ' : Optimizing the SVM Classifier.'))
  SVMDetails <- classify_optimize_svm(competitionID = competitionID,
                                      totalData = totalData,
                                      seasonStarting = seasonStarting,
                                      testData = testData,
                                      binList = binList,
                                      returnItems = commentaryNames,
                                      matchFieldNames = matchFieldNames,
                                      testing = TRUE)
 
  # Predict actual future results
  print(paste0(Sys.time(), ' : Predicting actual upcoming fixtures.'))
  classify_predict_matches(competitionID = competitionID,
                           competitionName = competitionName,
                           seasonStarting = seasonStarting,
                           returnItems = returnItems,
                           matchFieldNames = matchFieldNames,
                           subsetItems = SVMDetails[[2]],
                           SVMfit = SVMDetails[[1]], 
                           binList = binList)
}
