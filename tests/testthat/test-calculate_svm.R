context('test-classify_calculate_svm.R')

competitionID <- 1204
seasonStarting <- 2017
rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()

test_that('Calculate SVM from the test match data', {

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

  # Check the keyNames from the current list of commentarys.
  commentaryNames <- competitionID %>%
    footballstats::available_commentaries()

  # Calculate the feature set
  totalData <- footballstats::calculate_svm(
    commentaryNames = commentaryNames,
    matchData = matchData)

  expect_that( totalData %>% nrow, equals(40) )
  expect_that( totalData %>% names %>% length, equals(11) )
  expect_that( totalData$shots_total %>% range, equals(c(-22, 20)) )
  expect_that( totalData$shots_ongoal %>% range, equals(c(-11, 10)) )
  expect_that( totalData$fouls %>% range, equals(c(-10, 7)) )
  expect_that( totalData$corners %>% range, equals(c(-7, 9)) )
  expect_that( totalData$offsides %>% range, equals(c(-5, 8)) )
  expect_that( totalData$possesiontime %>% range, equals(c(-56, 56)) )
  expect_that( totalData$yellowcards %>% range, equals(c(-3, 3)) )
  expect_that( totalData$redcards %>% range, equals(c(-1, 1)) )
  expect_that( totalData$saves %>% range, equals(c(-7, 8)) )

  uniqueResults <- totalData$res %>% unique

  expect_that( 'L' %in% uniqueResults, is_true() )
  expect_that( 'D' %in% uniqueResults, is_true() )
  expect_that( 'W' %in% uniqueResults, is_true() )

})
