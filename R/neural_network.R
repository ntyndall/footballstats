#' @title Generate Artifical Neural Network
#'
#' @param trainingData
#' @export


neural_network <- function(totalData) {

  # Check what labels are available, and how many
  totalData$res %<>% as.character
  unlabel <- totalData
  unlabel$res <- NULL

  # Convert to integers (3 is the standard non-feature part of the data set)
  totCols <- ncol(unlabel)
  features <- unlabel[ , 1:totCols] %>% names

  # Convert malicious and normal to 0 or 1 and bind to numerical scaled data
  newLabels <- totalData$res %>% unique %>% sort
  trafficType <- totalData$res %>% as.factor %>% as.numeric %>% `-`(1)
  trafficType %<>% nnet::class.ind()
  dSet <- cbind(trafficType, unlabel)

  # Relabel the data set with the required classes
  names(dSet) <- newLabels %>% c(dSet[4:ncol(dSet)] %>% names)

  # Create Split (any column is fine)
  split.data <- dSet$W %>% caTools::sample.split(SplitRatio = 0.70)

  # Split based off of split Boolean Vector
  train.data <- dSet %>% subset(split.data %>% `==`(TRUE))
  test.data <- dSet %>% subset(split.data %>% `==`(FALSE))

  # Concatenate strings : Create the formula by adding
  # them up and create symbolic formula
  f <- paste0(
    newLabels %>% paste(collapse = ' + '), ' ~',
    paste(features, collapse = ' + ')) %>%
    as.formula

  # Calculate number of neurons
  neurons <- features %>%
    length %>%
    `+`(newLabels %>% length) %>%
    `/`(2) %>%
    round %>%
    `+`(1)

  # Build the neural network
  print(' ## Building neural network ## ')
  nn <- neuralnet::neuralnet(
    formula = f,
    data = train.data,
    hidden = neurons %>% rep(1),
    act.fct = "tanh",
    linear.output = FALSE,
    lifesign = 'minimal',
    stepmax = 10000000)

  # Compute Predictions off Test Set
  predictions <- neuralnet::compute(
    x = nn,
    covariate = test.data[(newLabels %>% length + 1):ncol(test.data)])

  # Create vectors to measure accuracy
  realVec <- predVec <- 0 %>% rep(test.data %>% nrow)
  tot <- 0
  for (i in 1:(newLabels %>% length)) {
    current <- test.data[[newLabels[i]]]
    realVec[current %>% `==`(1) %>% which] <- i
  }

  # Check the max values per row for the predictions
  netRes <- predictions$net.result
  for (j in 1:(netRes %>% nrow)) predVec[j] <- netRes[j, ] %>% which.max

  # Build a table of results
  myT <- table(realVec, predVec)
  if (myT %>% dim %>% unique %>% length %>% `>`(1)) stop(' ## Dimensions dont match.')

  # Print the confusion matrix of results
  print(' ## Confusion matrix ##')
  print(caret::confusionMatrix(data = myT))

  # Return the neural network
  return(nn)
}
