context('test-classify_claculate_data.R')

# Reset DB
rredis::redisFlushDB()

test_that('Calculate data set built from features', {

  matchData <- footballstats::amatch_info(
    competitionID = competitionID,
    dateFrom = NULL,
    dateTo = NULL,
    seasonStarting = seasonStarting,
    KEYS = KEYS
  )

  # Need to recreate it as new dates are created
  matchData <- footballstats::recreate_matchdata(
    competitionID = competitionID,
    seasonStarting = seasonStarting,
    matchLimit = 1000
  )

  footballstats::acommentary_info(
    competitionID = competitionID,
    matchIDs = matchData$id,
    localteam = matchData$localteam_id,
    visitorteam = matchData$visitorteam_id,
    KEYS = KEYS
  )

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

  expect_equal( totalData %>% nrow, 40 )
  expect_equal( totalData %>% names %>% length, 11 )
  expect_equal( totalData$shots_total %>% range, c(-22, 20) )
  expect_equal( totalData$shots_ongoal %>% range, c(-11, 10) )
  expect_equal( totalData$fouls %>% range, c(-10, 7) )
  expect_equal( totalData$corners %>% range, c(-7, 9) )
  expect_equal( totalData$possesiontime %>% range, c(-56, 56) )
  expect_equal( totalData$yellowcards %>% range, c(-3, 3) )
  expect_equal( totalData$saves %>% range, c(-7, 8) )
  expect_equal( totalData$form %>% range, c(-4, 5) )
  expect_equal( totalData$relativePos %>% range, c(-18, 19) )

  uniqueResults <- totalData$res %>% unique

  expect_that( 'L' %in% uniqueResults, is_true() )
  expect_that( 'D' %in% uniqueResults, is_true() )
  expect_that( 'W' %in% uniqueResults, is_true() )

})
