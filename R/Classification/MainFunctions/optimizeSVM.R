#' @title Optimize SVM
#'
#' @description A function that loops through all possibilities of the variables
#'  to be included in the classifier. That means the recent form and results 
#'  will be better represented as the best classifier will be built on the most
#'  recent results.
#'  
#' @param totalData A dataframe containing the data used by the classifier with
#'  variables matching those of `binList`.
#' @param testData A dataframe containing the test set used to find the results
#'  of the classification by comparing with the results in the testData.
#' @param binList A list of current interval values for the allowed variables used
#'  for building the classifier.
#' @param returnItems A vector of character values that hold the names of
#'  fields to be returned for the commentary statistics. 
#' @param testing A boolean value which decides whether to read the actual result
#'  of the match and compare with the classifier.
#'
#' @return A list containing the best SVM calculated in the first key, and 
#'  the best factors to use in the second key.


optimizeSVM <- function(competitionID, totalData, seasonStarting, testData, matchData, binList, 
                        returnItems, matchFieldNames, testing = TRUE) {

  # Optimize classifier by varying all the possible attributes
  bLLength <- length(binList)
  bestResult <- overtake <- 0
  vec <- c(1:bLLength)
  for (i in 1:bLLength) {
    for (j in 1:bLLength) {
      holdingList <- binList
      holdData <- totalData
      if (i <= j) {
        remove <- vec[i:j]
        if (length(remove) > (bLLength - 2)) { 
          next 
        }
        
        #print(paste0(Sys.time(), ' : Scanning through ', i, ' - ', j, '.'))
        
        # Delete rows and subset to vary the classification classes
        holdingList[remove] <- NULL
        holdData <- holdData[ , c(names(holdingList), 'res')]

        # Build and tune an SVM
        SVMfit <- calculateBestSVMFit(totalData = holdData)
        holdData$res <- NULL
        
        # Build and tune an SVM
        currentResult <- generatePredictions(competitionID = competitionID,
                                             fixtureList = testData, 
                                             seasonStarting = seasonStarting,
                                             testing = testing, 
                                             returnItems = returnItems,
                                             subsetItems = names(holdingList),
                                             SVMfit = SVMfit, 
                                             binList = holdingList,
                                             matchFieldNames = matchFieldNames)

        # If the current result is better than assign that to be the new SVM.
        if (currentResult > bestResult) {
          stri <- paste0(paste(rep(' ', (overtake * 3) + 1), collapse = ''), '|_')
          overtake <- overtake + 1
          print(paste0(Sys.time(), ' :', stri, ' New best result - ', currentResult, ' (from ', bestResult, ')'))
          bestResult <- max(currentResult, bestResult)
          bestFactors <- names(holdingList)
          bestSVM <- SVMfit
        }
      }
    }
  }
  return(list(bestSVM, bestFactors))
}
