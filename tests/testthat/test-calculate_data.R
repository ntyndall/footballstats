context('test-classify_claculate_data.R')

# Reset DB
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
    competitionID = competitionID,
    matchIDs = matchData$id,
    localteam = matchData$localteam_id,
    visitorteam = matchData$visitorteam_id,
    KEYS = KEYS)

  # Build league table
  matchData %>% footballstats::create_table()

  # Store positions on a weekly basis
  footballstats::weekly_positions(
    competitionID = competitionID,
    seasonStarting = seasonStarting
  )

  # Calculate the feature set
  totalData <- matchData %>% footballstats::calculate_data()
  totalData <- totalData[totalData %>% is.na %>% rowSums %>% `==`(0), ]

  expect_that( totalData %>% nrow, equals(40) )
  expect_that( totalData %>% names %>% length, equals(12) )
  expect_that( totalData$shots_total %>% range, equals(c(-22, 20)) )
  expect_that( totalData$shots_ongoal %>% range, equals(c(-11, 10)) )
  expect_that( totalData$fouls %>% range, equals(c(-10, 7)) )
  expect_that( totalData$corners %>% range, equals(c(-7, 9)) )
  expect_that( totalData$possesiontime %>% range, equals(c(-56, 56)) )
  expect_that( totalData$yellowcards %>% range, equals(c(-3, 3)) )
  expect_that( totalData$saves %>% range, equals(c(-7, 8)) )
  expect_that( totalData$form %>% range, equals(c(-4, 5)) )
  expect_that( totalData$convince %>% range, equals(c(-4, 5)) )
  expect_that( totalData$relativePos %>% range, equals(c(-18, 19)) )

  uniqueResults <- totalData$res %>% unique

  expect_that( 'L' %in% uniqueResults, is_true() )
  expect_that( 'D' %in% uniqueResults, is_true() )
  expect_that( 'W' %in% uniqueResults, is_true() )

})
