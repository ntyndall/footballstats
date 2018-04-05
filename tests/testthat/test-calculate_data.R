context('test-classify_claculate_data.R')

# Reset DB
rredis::redisFlushDB()

test_that('Calculate data set built from features', {

  # Add the match data to redis
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

  expect_equal( totalData %>% nrow, 20 )
  expect_equal( totalData %>% names %>% length, 20 )

  # range to int
  ri <- function(x) x %>% range %>% as.integer

  # Home details
  expect_equal( totalData$shots_total.h %>% ri(), c(7, 17) )
  expect_equal( totalData$shots_ongoal.h %>% ri(), c(2, 6) )
  expect_equal( totalData$fouls.h %>% ri(), c(7, 13) )
  expect_equal( totalData$corners.h %>% ri(), c(2, 9) )
  expect_equal( totalData$possesiontime.h %>% ri(), c(37, 64) )
  expect_equal( totalData$yellowcards.h %>% ri(), c(0, 3) )
  expect_equal( totalData$saves.h %>% ri(), c(1, 4) )
  expect_equal( totalData$form.h %>% ri(), c(1, 6) )
  expect_equal( totalData$position.h %>% ri(), c(1, 20) )

  # Away details
  expect_equal( totalData$shots_total.a %>% ri(), c(8, 18) )
  expect_equal( totalData$shots_ongoal.a %>% ri(), c(1, 6) )
  expect_equal( totalData$fouls.a %>% ri(), c(7, 12) )
  expect_equal( totalData$corners.a %>% ri(), c(3, 9) )
  expect_equal( totalData$possesiontime.a %>% ri(), c(40, 68) )
  expect_equal( totalData$yellowcards.a %>% ri(), c(0, 2) )
  expect_equal( totalData$saves.a %>% ri(), c(0, 4) )
  expect_equal( totalData$form.a %>% ri(), c(1, 6) )
  expect_equal( totalData$position.a %>% ri(), c(1, 20) )

  uniqueResults <- totalData$res %>% unique

  expect_true( 'L' %in% uniqueResults )
  expect_true( 'D' %in% uniqueResults )
  expect_true( 'W' %in% uniqueResults )

})
