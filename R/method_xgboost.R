#' @title Method XGBoost
#'
#' @export


method_xgboost <- function(total.results, odds.results, FOLD_DATA, XGB) {

  # Convert to integer values
  new.results <- total.results
  newLabels <- new.results$res %>% levels
  new.results$res %<>% as.factor %>% as.integer %>% `-`(1)

  # Convert new results to integer values
  boundLen <- 4
  bounds <- seq(0, 1, length.out = boundLen)
  for (i in 1:(new.results %>% nrow)) {
    for (j in 1:(new.results %>% ncol %>% `-`(1))) {
      new.results[i, j] %<>% findInterval(bounds)
    }
  }

  # How many folds per test set
  foldGroupLen <- FOLD_DATA$NUM - FOLD_DATA$PER

  # Loop through all the folds
  foldInd <- 1:(FOLD_DATA$NUM)

  # Initialise confusion matrix stats
  totalStats <- footballstats::init_conf_stats()

  # Start logging
  cat(" ## XG CV :")

  # Build the model
  for (i in 1:(FOLD_DATA$PER + 1)) {

     # Print out to see the progress
    cat(i, "/")
    if (i == (FOLD_DATA$PER + 1)) cat("\n")

    # Which indexes of the folds to include
    filterTest <- seq(
      from = i,
      by = 1,
      length.out = foldGroupLen
    )

    # Set up train and test data
    train.data <- new.results[
      FOLD_DATA$FOLDS[foldInd[-filterTest]] %>% purrr::flatten_int(), ]
    test.data <- new.results[
      FOLD_DATA$FOLDS[filterTest] %>% purrr::flatten_int(), ]
    new.odds <- odds.frame[
      FOLD_DATA$FOLDS[filterTest] %>% purrr::flatten_int(), ]

    # Create labels
    trainLabels <- train.data$res
    testLabels <- test.data$res

    # Create sparse matrix of training data
    sparse.train <- train.data %>%
      footballstats::create_sparse(
        boundLen = boundLen
      )

    # Create sparse test matrix
    sparse.test <- test.data %>%
      footballstats::create_sparse(
        boundLen = boundLen
      )

    # Build xgboost model
    xgb <- xgboost::xgboost(
      data = sparse.train,
      label = trainLabels,
      max_depth = XGB$DEPTH,
      eta = XGB$ETA,
      gamma = XGB$GAMMA,
      nthread = 2,
      nrounds = XGB$ROUNDS,
      objective = "multi:softmax",
      num_class = newLabels %>% length,
      verbose = 0
    )

    # Make predictions
    p <- predict(xgb, sparse.test)

    # Get metrics from confusion table
    totalStats %<>% footballstats::append_conf_stats(
      new.odds = new.odds,
      Actual.score = newLabels[testLabels %>% `+`(1)] %>% factor(levels = newLabels),
      Predicted.score = newLabels[p %>% `+`(1)] %>% factor(levels = newLabels)
    )
  }

  # Return neural network plus results
  return(totalStats)
}

