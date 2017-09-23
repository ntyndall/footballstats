





testOutDetails <- function(totalData, testData, SVMFit, subsetItems, allItems) {

  # Need to finish off this at some point..
  # Delete rows and subset to vary the classification classes
  holdingList[remove] <- NULL
  holdData <- subset(totalData, select = c(names(holdingList), 'res'))

  # Build and tune an SVM
  currentResult <- generateTestStats(binList = holdingList,
                                     testData = testData,
                                     SVMFit = SVMFit)
}
