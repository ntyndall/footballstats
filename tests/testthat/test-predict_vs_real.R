context("test-predict_vs_real.R")

# Reset DB
KEYS$RED$FLUSHDB()

test_that("Test a predicted result can be recorded as true or false.", {

  matchData <- footballstats::matchData[1, ]
  keyType <-  paste0('csdm_pred:', KEYS$COMP, ':', KEYS$SEASON, ':')

  actualKey <- paste0(keyType, '1:', matchData$id)
  actualKey %>% KEYS$RED$HMSET(
    field = c("home", "away", "prediction"),
    value = list("W", "L", "-")
  )

  # Need to add the match ID to the Redis set
  "all_predictions" %>% KEYS$RED$SADD(
    member = matchData$id
  )

  # Not check off the matchID
  readyToAnalyse <- actualKey %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr()

  KEYS %>% footballstats::predict_vs_real(
    readyToAnalyse = readyToAnalyse,
    matches = matchData
  )

  result <- actualKey %>% KEYS$RED$HGET(
    field = "prediction"
  )

  expect_equal( "all_predictions" %>% KEYS$RED$SMEMBERS() %>% length, 0 )
  expect_equal( result, "T" )

  # Add a new

})

test_that("Really simple check that a report can be generated", {

  KEYS$RED$FLUSHDB()

  # Set a dummy key
  'csdm_pred:1204:2017:2:1' %>% KEYS$RED$HSET(
    field = "prediction",
    value = "T"
  )

  res <- KEYS %>% footballstats::monthly_report(
    month = 2,
    year = KEYS$SEASON
  )

  expect_equal( res, NULL )

})
