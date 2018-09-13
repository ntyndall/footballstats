context('test-classify_all.R')

# Reset DB
KEYS$RED$FLUSHDB()


test_that('Classify all - end to end from adding data to classifying and predicting.', {

  # Get data ready
  raw.dat <- footballstats::data.2017 %>%
    subset(footballstats::data.2017$comp_id %>% `==`("1204"))

  # NEED TO UPDATE DATE
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

  # Load XGBoost model
  load(file = getwd() %>% paste0("/xgModel.rda"))

  # # Create the predictions here
  KEYS$LOG_PRED <- TRUE
  KEYS %>% predict_matches(
    datModel = xgModel,
    test.dat
  )
  KEYS$LOG_PRED <- FALSE

  predictions <- 'csdm_pred:1204:*' %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr() %>%
    strsplit(split = '[:]') %>%
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

})
