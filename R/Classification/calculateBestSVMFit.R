#'

calculateBestSVMFit <- function(totalData) {
  # Build SVM (Subset totalData to find useful classes)
  newData <- totalData
  newData$res <- NULL
  fitOne <- svm(totalData$res ~ ., 
                data = newData, 
                type = 'C-classification', 
                kernel = 'radial')
  predOne = predict(fitOne, newData)
  firstResults <- table(predOne, totalData$res)
  
  # Tune SVM
  tuningParameters <- tune(method = svm,
                           train.x = newData, 
                           train.y = as.factor(totalData$res),
                           kernel = 'radial', 
                           ranges = list(cost = 2^(2:9), 
                                         gamma = seq(0.1, 2, 0.1)))

  # Create a new SVM based on tuning parameters
  fitTwo <- svm(totalData$res ~ ., 
                data = newData, 
                type = 'C-classification', 
                kernel = 'radial',
                cost = tuningParameters$best.parameters$cost, 
                gamma = tuningParameters$best.parameters$gamma)
  predTwo = predict(fitTwo, newData)
  secondResults <- table(predTwo, totalData$res)
  
  # Return the best from normal and fit SVM's
  return(c(sum(firstResults[c(1, 5, 9)]), sum(secondResults[c(1, 5, 9)])) %>% purrr::when(.[1] >= .[2] ~ fitOne,
                                                                                          ~ fitTwo))
}
