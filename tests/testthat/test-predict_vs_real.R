context("test-predict_vs_real.R")

rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()
competitionID <- 1204

test_that("Test a predicted result can be recorded as true or false.", {

  matchData <- footballstats::matchData[1, ]
  keyType <-  paste0('c:', competitionID, ':pred:')

  rredis::redisHMSet(
    key = paste0(keyType, matchData$id),
    values = list(home = 'W', away = 'L'))

  readyToAnalyse <- paste0(keyType, '*') %>% rredis::redisKeys()
  predict_vs_real(
    competitionID = competitionID,
    readyToAnalyse = readyToAnalyse,
    matches = matchData )

  result <- rredis::redisHGet(
    key = paste0(keyType, matchData$id),
    field = 'prediction') %>% as.character

  expect_that( result, equals('T') )

})
