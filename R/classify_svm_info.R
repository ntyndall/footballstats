#'
#' @export


best_svm <- function(totalData) {
  # Split data into actual data and one similar minus the results
  newData <- totalData
  newData$res <- NULL

  # Create a basic SVM with no tuning
  fitOne <- e1071::svm(
    totalData$res ~ .,
    data = newData,
    type = 'C-classification',
    kernel = 'radial')

  predOne = stats::predict(fitOne, newData)
  firstResults <- table(predOne, totalData$res)

  # Tune SVM
  tuningParameters <- e1071::tune(
    method = svm,
    train.x = newData,
    train.y = as.factor(totalData$res),
    kernel = 'radial',
    ranges = list(cost = 2^(2:9),
                  gamma = seq(0.1, 2, 0.1)))

  # Create a new SVM based on tuning parameters
  fitTwo <- e1071::svm(
    totalData$res ~ .,
    data = newData,
    type = 'C-classification',
    kernel = 'radial',
    cost = tuningParameters$best.parameters$cost,
    gamma = tuningParameters$best.parameters$gamma)

  predTwo = stats::predict(fitTwo, newData)
  secondResults <- table(predTwo, totalData$res)

  # Return the best from normal and fit SVM's
  return(c(sum(firstResults[c(1, 5, 9)]),
           sum(secondResults[c(1, 5, 9)])) %>%
             purrr::when(.[1] >= .[2] ~ fitOne, ~ fitTwo))
}


#' @title optimize_svm
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
#'
#' @export


optimize_svm <- function(competitionID, totalData, seasonStarting, testData, matchData,
                         binList, returnItems, matchFieldNames, testing = TRUE) {

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
        SVMfit <- footballstats::best_svm(
          totalData = holdData)
        holdData$res <- NULL

        # Build and tune an SVM
        currentResult <- footballstats::generate_predictions(
          competitionID = competitionID,
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

