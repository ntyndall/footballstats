
calculateBestSVMFit <- function(totalData) {
  # Build SVM
  newData <- totalData
  
  # Change form to a number!!!
  for (i in 1:nrow(newData)) {
    wld <- strsplit(newData$form[i], '')[[1]]
    newData$form[i] <- as.integer((sum(wld == "W")*2) + sum(wld == "D"))
  }
  newData$res <- NULL
  newData$form <- as.double(newData$form)
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
                           ranges = list(cost=10^(-1:2), 
                                         gamma=c(.5,1,2)))

  fitTwo <- svm(totalData$res ~ ., 
                data = newData, 
                type = 'C-classification', 
                kernel = 'radial',
                cost = tuningParameters$best.parameters$cost, 
                gamma = tuningParameters$best.parameters$gamma)

  predTwo = predict(fitTwo, newData)
  secondResults <- table(predTwo, totalData$res)
  
  one <- sum(firstResults[c(1, 5, 9)])
  two <- sum(secondResults[c(1, 5, 9)])
  if (one >= two) {
    return(fitOne)
  } else {
    return(fitTwo)
  }
}
