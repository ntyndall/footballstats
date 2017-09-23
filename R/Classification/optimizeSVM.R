


optimizeSVM <- function(totalData, testData, binList, testOnData = FALSE) {
  # Optimize classifier by varying all the possible attributes
  bLLength <- length(binList)
  bestResult <- 0
  vec <- c(1:bLLength)
  for (i in 1:bLLength) {
    for (j in 1:bLLength) {
      holdingList <- binList
      holdData <- totalData
      if (i <= j) {
        remove <- vec[i:j]
        if (length(remove) == 8) { 
          next 
        }
        
        # Delete rows and subset to vary the classification classes
        holdingList[remove] <- NULL
        holdData <- subset(totalData, select = c(names(holdingList), 'res'))
        
        # Build and tune an SVM
        SVMFit <- calculateBestSVMFit(totalData = holdData)
        
        # Build and tune an SVM
        currentResult <- generateTestStats(binList = holdingList,
                                           testData = testData,
                                           SVMFit = SVMFit)
        if (currentResult > bestResult) {
          print(paste0('New best result! .. ', currentResult, ' (overtaking ', bestResult, ')'))
          bestResult <- max(currentResult, bestResult)
          bestFactors <- names(holdingList)
          bestSVM <- SVMFit
        }
      }
    }
  }
  return(list(bestSVM, bestFactors))
}
