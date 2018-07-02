#' @title Generate Artifical Neural Network
#'
#' @description A function that takes a scaled data set
#'  builds a neural network and reports on the accuracy
#'  of the built model.
#'
#' @param totalData A data set that contains scaled data
#'  and a vector of results as a column in the data frame.
#'
#' @return A neural network model built from \code{totalData}
#'  which is of class 'nn'.
#'
#' @export


neural_network <- function(totalData, NN) {

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
  labColSpace <- newLabels %>% length %>% `+`(1)
  names(dSet) <- newLabels %>% c(dSet[labColSpace:ncol(dSet)] %>% names)

  # totalData$res[totalData$res == 'W'] <- 'L'
  totalRows <- dSet %>% nrow
  test.data <- train.data <- data.frame(stringsAsFactors = FALSE)
  for (k in newLabels) {
    dataByClass <- dSet[dSet[[k]] %>% `==`(1), ]
    splitting <- dataByClass[[k]] %>% caTools::sample.split(0.7)


    #logicalVec <- FALSE %>% rep(dataByClass %>% nrow)
    #logicalVec[sample(c(1:(dataByClass %>% nrow)), 430)] <- TRUE

    train.data %<>% rbind(dataByClass %>% subset(splitting %>% `==`(TRUE)))
    test.data %<>% rbind(dataByClass %>% subset(splitting %>% `==`(FALSE)))
   }

  # train.data[sample(1:nrow(train.data)), ] -> train.data
  # Create Split (any column is fine)
  #splitting <- dSet$W %>% caTools::sample.split(SplitRatio = 0.70)

  #split.data <- splitting
  # Split based off of split Boolean Vector
  #train.data <- dSet %>% subset(splitting %>% `==`(TRUE))
  #test.data <- dSet %>% subset(splitting %>% `==`(FALSE))

  # Concatenate strings : Create the formula by adding
  # them up and create symbolic formula
  f <- paste0(
    newLabels %>% paste(collapse = ' + '), ' ~',
    paste(features, collapse = ' + ')) %>%
    stats::as.formula()

  # Calculate number of neurons
  neurons <- features %>%
    length %>%
    `+`(newLabels %>% length) %>%
    `/`(2) %>%
    round %>%
    `+`(1)

  # Build the neural network 0.0000001
  # print(' ## Building neural network ## ')
  nn <- neuralnet::neuralnet(
    formula = f,
    data = train.data,
    hidden = neurons %>% rep(3),
    rep = NN$REP,
    threshold = NN$THRESH,
    act.fct = "logistic",
    linear.output = FALSE,
    lifesign = 'none',
    stepmax = 10000000
  )

  # Compute Predictions off Test Set
  predictions <- neuralnet::compute(
    x = nn,
    covariate = test.data[(newLabels %>% length + 1):ncol(test.data)]
  )

  # Create vectors to measure accuracy
  realVec <- predVec <- 0 %>% rep(test.data %>% nrow)
  tot <- 0
  for (i in 1:(newLabels %>% length)) {
    current <- test.data[[newLabels[i]]]
    realVec[current %>% `==`(1) %>% which] <- newLabels[i]
  }

  # Check the max values per row for the predictions
  netRes <- predictions$net.result
  for (j in 1:(netRes %>% nrow)) predVec[j] <- newLabels[netRes[j, ] %>% which.max]

  Actual.score <- realVec %>% factor(levels = newLabels)
  Predicted.score <- predVec %>% factor(levels = newLabels)

  # Build a table of results
  resultTable <- table(Actual.score, Predicted.score)
  #print(' ## Confusion matrix ##')
  rt <- caret::confusionMatrix(data = resultTable)
  #print(rt)

  # Return neural network plus results
  return(list(neural = nn, result = rt$overall[1]))
}
