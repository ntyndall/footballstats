context('test-classify_all.R')

# Reset DB
KEYS$RED$FLUSHDB()

test_that("No predictions to be made", {

  # If no data is found, then it will be assigned NULL
  noResults <- KEYS %>%
    footballstats::predict_matches(
      datModel = footballstats::nnModel,
      cMethod = "neuralnetwork",
      NULL
    )

  expect_equal( noResults, 0 )

})

test_that('Classify all - end to end from adding data to classifying and predicting (both methods).', {

  # Get data ready
  raw.dat <- footballstats::data.2017 %>%
    subset(footballstats::data.2017$comp_id %>% `==`("1204"))

  # NEED TO UPDATE DATE (otherwise handled in amatch_info)
  raw.dat$formatted_date %<>% format("%d.%m.%Y")

  train.dat <- raw.dat[1:359, ]
  test.dat <- raw.dat[360:370, ]
  com.metrics <- footballstats::total.metrics[1:(train.dat %>% nrow), ]

  # Add data to redis
  match.data <- KEYS %>% footballstats::amatch_info(train.dat)

  # Add commentaries
  com.metrics %>% footballstats::comm_from_metrics()

  # Order the match data
  match.data %<>% footballstats::order_matchdata()

  # Set up league information for predictions
  KEYS %>%
    footballstats::create_table(
      matchData = match.data
    )
  KEYS %>% footballstats::weekly_positions()

  # Create the predictions here
  KEYS$LOG_PRED <- TRUE

  # Set up grids
  GRIDS <- list(
    DAYS = 3,
    GRID_PTS = 8,
    GRID_BOUND = 0.2,
    DECAY = 5000,
    TOTAL_PERC = 1,
    NN_REP = 1,
    NN_THRESH = 0.005,
    XG_ROUNDS = 100,
    XG_DEPTH = 10,
    XG_ETA = 0.2,
    XG_GAMMA = 2
  )

  # Loop over the two different methods here
  for (i in 1:2) {
    # Choose the right model
    datModel <- if (i == 1) {
      footballstats::nnModel
    } else {
      footballstats::total.metrics[1:300, ] %>%
        footballstats::optimize_variables(
          GRIDS = GRIDS,
          optimizeModels = FALSE,
          types = "xgboost"
        ) %>%
        `[[`("xgb") %>%
        `[[`("model")
    }

    KEYS %>% footballstats::predict_matches(
      datModel = datModel,
      cMethod = if (i == 1) "neuralnetwork" else "xgboost",
      test.dat
    )

    predictions <- "csdm_pred:1204:*" %>%
      KEYS$RED$KEYS() %>%
      purrr::flatten_chr() %>%
      strsplit(split = "[:]") %>%
      purrr::map(5) %>%
      purrr::flatten_chr() %>%
      as.integer %>%
      sort

    # Number of predictions match up?
    expect_equal( predictions %>% length, test.dat %>% nrow )

    # Make sure all the IDs match up
    matchIDs <- test.dat$id %>% as.integer %>% sort
    for (j in 1:(test.dat %>% nrow)) expect_equal( predictions[j], matchIDs[j] )
  }

  KEYS$LOG_PRED <- FALSE
  # Reset db
  KEYS$RED$FLUSHDB()

})


test_that('Classify all - end to end from adding data to classifying and predicting (xgboost).', {

  # Get data ready
  raw.dat <- footballstats::data.2017 %>%
    subset(footballstats::data.2017$comp_id %>% `==`("1204"))

  # NEED TO UPDATE DATE (otherwise handled in amatch_info)
  raw.dat$formatted_date %<>% format("%d.%m.%Y")

  train.dat <- raw.dat[1:359, ]
  test.dat <- raw.dat[360:370, ]
  com.metrics <- footballstats::total.metrics[1:(train.dat %>% nrow), ]

  # Add data to redis
  match.data <- KEYS %>% footballstats::amatch_info(train.dat)

  # Add commentaries
  com.metrics %>% footballstats::comm_from_metrics()

  # Order the match data
  match.data %<>% footballstats::order_matchdata()

  # Set up league information for predictions
  KEYS %>%
    footballstats::create_table(
      matchData = match.data
    )
  KEYS %>% footballstats::weekly_positions()

  # Prepare the xgboost model
  GRIDS <- list(
    DAYS = 3,
    GRID_PTS = 8,
    GRID_BOUND = 0.2,
    DECAY = 5000,
    TOTAL_PERC = 1,
    NN_REP = 1,
    NN_THRESH = 0.005,
    XG_ROUNDS = 100,
    XG_DEPTH = 10,
    XG_ETA = 0.2,
    XG_GAMMA = 2
  )

  # Build a small xgboost model..
  res <- footballstats::total.metrics[1:300, ] %>%
    footballstats::optimize_variables(
      GRIDS = GRIDS,
      optimizeModels = FALSE,
      types = "xgboost"
    )

  # Create the predictions here
  KEYS$LOG_PRED <- TRUE
  KEYS %>% footballstats::predict_matches(
    datModel = res$xgb$model,
    cMethod = "xgboost",
    test.dat
  )
  KEYS$LOG_PRED <- FALSE

  predictions <- "csdm_pred:1204:*" %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr() %>%
    strsplit(split = "[:]") %>%
    purrr::map(5) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  # Number of predictions match up?
  expect_equal( predictions %>% length, test.dat %>% nrow )

  # Make sure all the IDs match up
  matchIDs <- test.dat$id %>% as.integer %>% sort
  for (i in 1:(test.dat %>% nrow)) {
    expect_equal( predictions[i], matchIDs[i] )
  }

  # Reset db
  KEYS$RED$FLUSHDB()

})
