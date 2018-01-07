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
    method = 'svm',
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


optimize_svm <- function(totalData) {

  # Sample the data sets
  split <- totalData$res %>% caTools::sample.split(SplitRatio = 0.70)
  trainData <- totalData %>% subset(`==`(split, TRUE))
  testData <- totalData %>% subset(`==`(split, FALSE))

  # Initialise features to consider
  feats <- testData %>% names
  feats %<>% subset(feats != 'res')
  len <- feats %>% length
  bestAcc <- overtake <- 0

  # Optimize classifier by varying all the possible attributes
  vec <- c(1:len)
  for (i in 1:len) {
    for (j in 1:len) {
      holdFeats <- feats
      holdData <- trainData
      if (i <= j) {
        remove <- vec[i:j]
        if (length(remove) > (len - 2)) next

        # Build and tune an SVM
        SVMfit <- holdData[ ,c(holdFeats[-remove], 'res')] %>%
          footballstats::best_svm()
        svmPredictions <- stats::predict(SVMfit, testData)

        # Get the accuracy on the confusion matrix
        myT <- table(testData$res, svmPredictions)
        confuseMat <- caret::confusionMatrix(data = myT)
        currentAcc <- confuseMat$overall[1] %>% as.numeric

        # If the current result is better than assign that to be the new SVM.
        if (currentAcc > bestAcc) {
          stri <- paste0(paste(rep(' ', (overtake * 2) + 1), collapse = ''), ' |_')
          currentAcc %<>% round(digits = 2)
          bestAcc %<>% round(digits = 2)
          cat(paste0(Sys.time(), stri, ' New best result - ', currentAcc, ' (from ', bestAcc, ') \n'))
          overtake %<>% `+`(1)
          bestAcc %<>% max(currentAcc)

          # Store the best results so far
          returningResults <- list(SVMfit, holdFeats)
        }
      }
    }
  }
  returningResults %>% return()
}

