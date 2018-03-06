context('test-classify_all.R')

# Reset DB
rredis::redisFlushDB()

test_that('Classify all - end to end from adding data to classifying and predicting.', {

  matchData <- KEYS %>% footballstats::amatch_info()

  KEYS %>% footballstats::acommentary_info(
    matchIDs = matchData$id,
    localteam = matchData$localteam_id,
    visitorteam = matchData$visitorteam_id
  )

  # Set up league information for predictions
  matchData %<>% footballstats::order_matchdata()
  matchData %>% footballstats::create_table()
  KEYS %>% footballstats::weekly_positions()

  # Create the predictions here
  KEYS$LOG_PRED <- TRUE
  KEYS %>% footballstats::predict_matches()
  KEYS$LOG_PRED <- FALSE

  predictions <- 'csdm_pred:1204:*' %>%
    rredis::redisKeys() %>%
    strsplit(split = '[:]') %>%
    purrr::map(5) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  expect_equal( predictions %>% length, 11 )
  expect_equal( predictions[1], 2212967 )
})
