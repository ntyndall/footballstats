context("test-predict_vs_real.R")

# Reset DB
rredis::redisFlushDB()

test_that("Test a predicted result can be recorded as true or false.", {

  matchData <- footballstats::matchData[1, ]
  keyType <-  paste0('csdm_pred:', competitionID, ':2017:')

  actualKey <- paste0(keyType, '1:', matchData$id)
  rredis::redisHMSet(
    key = actualKey,
    values = list(home = 'W', away = 'L', prediction = '-'))

  readyToAnalyse <- actualKey %>% rredis::redisKeys()
  footballstats::predict_vs_real(
    competitionID = competitionID,
    seasonStarting = seasonStarting,
    readyToAnalyse = readyToAnalyse,
    matches = matchData
  )

  result <- rredis::redisHGet(
    key = actualKey,
    field = 'prediction') %>% as.character

  expect_that( result, equals('T') )

})

test_that("Really simple check that a report can be generated", {

  # Set a dummy key
  'csdm_pred:1204:2017:2:1' %>% rredis::redisHMSet(
    values = list(prediction = 'T')
  )

  KEYS %>% footballstats::monthly_report(
    month = 2,
    year = 2017
  )

})
