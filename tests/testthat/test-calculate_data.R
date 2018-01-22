context('test-classify_claculate_data.R')

competitionID <- 1204
seasonStarting <- 2017
rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()

test_that('Calculate data set built from features', {

  matchData <- footballstats::amatch_info(
    competitionID = competitionID,
    dateFrom = NULL,
    dateTo = NULL,
    seasonStarting = seasonStarting,
    analysingToday = TRUE,
    KEYS = KEYS)

  # Need to recreate it as new dates are created
  matchData <- footballstats::recreate_matchdata(
    competitionID = competitionID,
    seasonStarting = seasonStarting,
    matchLimit = 1000)

  footballstats::acommentary_info(
    competitionID = 1204,
    matchIDs = matchData$id,
    localteam = matchData$localteam_id,
    visitorteam = matchData$visitorteam_id,
    KEYS = KEYS)

  # Calculate the feature set
  totalData <- matchData %>% footballstats::calculate_data()
  totalData <- totalData[totalData %>% is.na %>% rowSums %>% `==`(0), ]

  expect_that( totalData %>% nrow, equals(40) )
  expect_that( totalData %>% names %>% length, equals(11) )
  expect_that( totalData$shots_total %>% range, equals(c(-22, 20)) )
  expect_that( totalData$shots_ongoal %>% range, equals(c(-11, 10)) )
  expect_that( totalData$fouls %>% range, equals(c(-10, 7)) )
  expect_that( totalData$corners %>% range, equals(c(-7, 9)) )
  expect_that( totalData$possesiontime %>% range, equals(c(-56, 56)) )
  expect_that( totalData$yellowcards %>% range, equals(c(-3, 3)) )
  expect_that( totalData$saves %>% range, equals(c(-7, 8)) )

  uniqueResults <- totalData$res %>% unique

  expect_that( 'L' %in% uniqueResults, is_true() )
  expect_that( 'D' %in% uniqueResults, is_true() )
  expect_that( 'W' %in% uniqueResults, is_true() )

})
