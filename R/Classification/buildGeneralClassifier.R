



buildGeneralClassifier <- function(redisConnection, competitionID, matchData, seasonStarting,
                                   returnItems, matchLimit = 150) {

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
                                returnItems = returnItems)

  # Get the binning limits...
  binList <- getBinns(totalData = totalData)
  
  # apply it to the totalData...
  # Map current form to an integer value also.
  allForms <- strsplit(totalData$form, '')
  totalData$form <- sapply(1:length(allForms), function(x) {
    wld <- allForms[[x]]
    as.integer((sum(wld == "W")*2) + sum(wld == "D"))
  })

  binNames <- names(binList)
  for (i in 1:length(binList)) {
    singleBin <- binList[[binNames[i]]]
    vec <- totalData[[binNames[i]]]
    for (j in 1:bins) {
      vec[which(vec > (singleBin[j] - 1e-5) & vec <= singleBin[j+1])] <- j * (-1)
    }
    totalData[[binNames[i]]] <- vec
  }
  
  # Readjust thresholds of the integer valued metrics
  #totalData <- mapMetricsToThresholds(totalData = totalData)
  
  # Test the last match data...
  testData <- matchData[(nrow(matchData) - 9):nrow(matchData), ]
  totalData <- totalData[-c((nrow(totalData) - 18):nrow(totalData)), ]
  
  # Optimize the SVM by looping through all available variables
  SVMDetails <- optimizeSVM(totalData = totalData, 
                            testData = testData,
                            binList = binList)
  
  # Optional...
  testOutDetails(totalData = totalData, 
                 testData = testData,
                 SVMFit = SVMDetails[[1]],
                 subsetItems = SVMDetails[[2]],
                 allItems = returnItems)
  

  # Predict actual future results
  resultsPredicted <- predictFutureMatches(competitionID = competitionID,
                                           seasonStarting = seasonStarting,
                                           returnItems = SVMDetails[[2]],
                                           SVMfit = SVMDetails[[1]])
}
