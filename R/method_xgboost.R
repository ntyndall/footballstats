#' @title Method XGBoost
#'
#' @export


method_xgboost <- function(total.results, odds.results, FOLD_DATA, XGB) {

  # Initialise bestAcc
  bestAcc <- 0

  # Convert to integer values
  new.results <- total.results
  new.results$res %<>% as.factor
  newLabels <- new.results$res %>% levels
  new.results$res %<>% as.factor %>% as.integer %>% `-`(1)

  # Convert new results to integer values
  boundLen <- 4
  new.results %<>% footballstats::scaled_to_discrete(boundLen = boundLen)

  # How many folds per test set
  foldGroupLen <- FOLD_DATA$NUM - FOLD_DATA$PER

  # Loop through all the folds
  foldInd <- 1:(FOLD_DATA$NUM)

  # Initialise confusion matrix stats
  totalStats <- footballstats::init_conf_stats()

  # Start logging
  cat("\n ## XG CV :")

  # Build the model
  for (i in 1:(FOLD_DATA$PER + 1)) {

     # Print out to see the progress
    cat("", i, "/")
    if (i == (FOLD_DATA$PER + 1)) cat("\n")

    # Which indexes of the folds to include
    filterTest <- seq(
      from = i,
      by = 1,
      length.out = foldGroupLen
    )

    # Set up train and test data
    train.data <- new.results[FOLD_DATA$FOLDS[-filterTest] %>% purrr::flatten_int(), ]
    test.data <- new.results[FOLD_DATA$FOLDS[filterTest] %>% purrr::flatten_int(), ]
    new.odds <- odds.results[FOLD_DATA$FOLDS[filterTest] %>% purrr::flatten_int(), ]

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

    # Save the best model
    if (totalStats$totAcc[i] > bestAcc) {
      bestAcc <- totalStats$totAcc[i]
      bestXGB <- xgb
    }

    rm(xgb)
    rm(train.data)
    rm(test.data)
    rm(new.odds)
  }

  # Return neural network plus results
  return(c(list(model = bestXGB), totalStats))
}

