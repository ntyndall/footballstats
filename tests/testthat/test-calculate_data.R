context('test-classify_claculate_data.R')

# Reset DB
rredis::redisFlushDB()

test_that('Calculate data set built from features', {

  matchData <- KEYS %>% footballstats::amatch_info()

  # Need to recreate it as new dates are created
  matchData <- KEYS %>% footballstats::recreate_matchdata()

  KEYS %>% footballstats::acommentary_info(
    matchIDs = matchData$id,
    localteam = matchData$localteam_id,
    visitorteam = matchData$visitorteam_id
  )

  # Build league table
  matchData %>% footballstats::create_table()

  # Store positions on a weekly basis
  KEYS %>% footballstats::weekly_positions()

  # Calculate the feature set
  totalData <- matchData %>% footballstats::calculate_data()
  totalData <- totalData[totalData %>% is.na %>% rowSums %>% `==`(0), ]

  expect_equal( totalData %>% nrow, 40 )
  expect_equal( totalData %>% names %>% length, 20 )

  # Home details
  expect_equal( totalData$shots_total.h %>% range, c(4, 25) )
  expect_equal( totalData$shots_ongoal.h %>% range, c(0, 10) )
  expect_equal( totalData$fouls.h %>% range, c(2, 18) )
  expect_equal( totalData$corners.h %>% range, c(0, 11) )
  expect_equal( totalData$possesiontime.h %>% range, c(22, 78) )
  expect_equal( totalData$yellowcards.h %>% range, c(0, 4) )
  expect_equal( totalData$saves.h %>% range, c(0, 9) )
  expect_equal( totalData$form.h %>% range, c(0, 6) )
  expect_equal( totalData$position.h %>% range, c(1, 20) )

  # Away details
  expect_equal( totalData$shots_total.a %>% range, c(1, 30) )
  expect_equal( totalData$shots_ongoal.a %>% range, c(0, 14) )
  expect_equal( totalData$fouls.a %>% range, c(4, 20) )
  expect_equal( totalData$corners.a %>% range, c(0, 10) )
  expect_equal( totalData$possesiontime.a %>% range, c(22, 78) )
  expect_equal( totalData$yellowcards.a %>% range, c(0, 4) )
  expect_equal( totalData$saves.a %>% range, c(0, 7) )
  expect_equal( totalData$form.a %>% range, c(0, 6) )
  expect_equal( totalData$position.a %>% range, c(1, 20) )

  uniqueResults <- totalData$res %>% unique

  expect_true( 'L' %in% uniqueResults )
  expect_true( 'D' %in% uniqueResults )
  expect_true( 'W' %in% uniqueResults )

})
