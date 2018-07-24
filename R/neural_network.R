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


neural_network <- function(totalData, odds.results, FOLD_DATA, NN, LOGS = FALSE) {

  # Check what labels are available, and how many
  totalData$res %<>% as.character
  unlabel <- totalData
  unlabel$res <- NULL

  # Convert to integers
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

  # Concat strings, create the formula by adding up for symbolic formula
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

  # How many folds per test set
  foldGroupLen <- FOLD_DATA$NUM - FOLD_DATA$PER

  # Initialise list of vectors to save
  totalStats <- footballstats::init_conf_stats()

  # Build the neural network
  for (i in 1:(FOLD_DATA$PER + 1)) {

    # Print out to see the progress
    if (i == (FOLD_DATA$PER + 1)) cat("! \n") else if (i == 1) cat( "## NN CV : .") else cat(".")

    # Which indexes of the folds to include
    filterTest <- seq(
      from = i,
      by = 1,
      length.out = foldGroupLen
    )

    # Loop through all the folds
    foldInd <- 1:FOLD_DATA$NUM

    # Set up train and test data
    train.data <- dSet[
      FOLD_DATA$FOLDS[-filterTest] %>% purrr::flatten_int(), ]
    test.data <- dSet[
      FOLD_DATA$FOLDS[filterTest] %>% purrr::flatten_int(), ]
    new.odds <- odds.results[
      FOLD_DATA$FOLDS[filterTest] %>% purrr::flatten_int(), ]

    # Build the neural network with split data
    if (LOGS) cat(' ## Building neural network ## \n')

    # Calculate the NN here
    nn <- tryCatch({
      neuralnet::neuralnet(
        formula = f,
        data = train.data,
        hidden = neurons %>% rep(2),
        rep = NN$REP,
        threshold = NN$THRESH,
        act.fct = "logistic",
        linear.output = FALSE,
        lifesign = if (LOGS) "full" else "none",
        stepmax = 1000000 # Old = 10000000
      )
    }, warning = function(w) return(NULL)
    )

    # If the NN couldn't converge in time then move on
    if (nn %>% is.null) next

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

    # Get metrics from confusion table
    totalStats %<>% footballstats::append_conf_stats(
      new.odds = new.odds,
      Actual.score = realVec %>% factor(levels = newLabels),
      Predicted.score = predVec %>% factor(levels = newLabels)
    )
  }

  # Return neural network plus results
  return(c(totalStats, list(neural = nn)))
}
