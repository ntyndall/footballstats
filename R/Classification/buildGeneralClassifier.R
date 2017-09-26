#' @title Optimize SVM
#'
#' @description A function that loops through all possibilities of the variables
#'  to be included in the classifier. That means the recent form and results 
#'  will be better represented as the best classifier will be built on the most
#'  recent results.
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

  # Construct data set for building an SVM
  print(paste0(Sys.time(), ' : Creating a dataframe from the match data.'))
  totalData <- calculateSVMData(competitionID = competitionID,
                                seasonStarting = seasonStarting,
                                commentaryKeys = as.character(redisConnection$KEYS(pattern = 'cmt_commentary*')),
                                matchData = matchData,
                                returnItems = returnItems)

  # Get the binning limits
  binList <- getBinns(totalData = totalData)
  
  # Map current form to an integer value also.
  totalData$form <- mapFormToInteger(oldForms = totalData$form)
  binNames <- names(binList)
  for (i in 1:length(binList)) {
    singleBin <- binList[[binNames[i]]]
    vec <- totalData[[binNames[i]]]
    for (j in 1:bins) {
      vec[which(vec > (singleBin[j] - 1e-5) & vec <= singleBin[j+1])] <- j * (-1)
    }
    totalData[[binNames[i]]] <- vec
  }
  
  # Test the last match data...
  testData <- matchData[(nrow(matchData) - 9):nrow(matchData), ]
  totalData <- totalData[-c((nrow(totalData) - 18):nrow(totalData)), ]
  
  # Optimize the SVM by looping through all available variables
  print(paste0(Sys.time(), ' : Optimizing the SVM Classifier.'))
  SVMDetails <- optimizeSVM(totalData = totalData, 
                            testData = testData,
                            binList = binList, 
                            returnItems = returnItems,
                            testing = TRUE)
 
  # Predict actual future results
  print(paste0(Sys.time(), ' : Predicting actual upcoming fixtures.'))
  predictFutureMatches(competitionID = competitionID,
                       seasonStarting = seasonStarting,
                       returnItems = returnItems,
                       subsetItems = SVMDetails[[2]],
                       SVMfit = SVMDetails[[1]])
}
