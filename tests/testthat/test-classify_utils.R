context("test-classify_utils.R")

# Reset DB
rredis::redisFlushDB()

test_that("Test that match data can be recreated easily.", {

  # Test no data returns a null data frame
  recreated <- KEYS %>% footballstats::recreate_matchdata()

  expect_equal( recreated %>% nrow, 0 )

  # Put the test data into Redis
  matchData <- KEYS %>% footballstats::amatch_info()

  recreated <- KEYS %>% footballstats::recreate_matchdata()

  expect_equal( matchData %>% nrow, recreated %>% nrow )
  expect_equal( recreated %>% names, matchData %>% names %>% setdiff('events') )
  expect_equal( matchData$id %>% as.integer %>% sort, recreated$id %>% as.integer %>% sort )

})


test_that("Test that commentary data is sent to Redis.", {

  # Recreate the match data that is in redis
  recreated <- KEYS %>% footballstats::recreate_matchdata()

  # Choose the right match ID to analyse
  recreated <- recreated[recreated$id == '2212950', ]

  KEYS %>% footballstats::acommentary_info(
    matchIDs = recreated$id,
    localteam = recreated$localteam_id,
    visitorteam = recreated$visitorteam_id
  )

  # Check what keys have been added
  commentaryKeys <- paste0('cmt_commentary:', KEYS$COMP, '*') %>%
    rredis::redisKeys() %>%
    as.character

  expect_equal( commentaryKeys %>% length, 2 )
  # Produces a list of available commentary names, Must be a total intersection
  commentaryNames <- KEYS$COMP %>%
    footballstats::available_commentaries()

  expect_equal( commentaryNames %>% length, 9 )
  expect_true( 'shots_total' %in% commentaryNames )
  expect_true( 'shots_ongoal' %in% commentaryNames )
  expect_true( 'fouls' %in% commentaryNames )
  expect_true( 'corners' %in% commentaryNames )
  expect_true( 'offsides' %in% commentaryNames )
  expect_true( 'possesiontime' %in% commentaryNames )
  expect_true( 'yellowcards' %in% commentaryNames )
  expect_true( 'redcards' %in% commentaryNames )
  expect_true( 'saves' %in% commentaryNames )

})

test_that("Check that the commentaries can be retrieved from redis as a double vector", {

  commentaryKeys <- paste0('cmt_commentary:', KEYS$COMP, '*') %>%
    rredis::redisKeys() %>%
    as.character

  # Make sure to pick the right commentary for testing
  teamIDs <- commentaryKeys %>%
    strsplit(split = '[:]') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  keyName <- commentaryKeys[grepl(teamIDs[1], commentaryKeys) %>% which]
  result <- footballstats::commentary_from_redis(
    keyName = keyName,
    returnItems = 'saves'
  )

  expect_equal( result %>% length, 1 )
  expect_equal( result, 1 )

  result <- footballstats::commentary_from_redis(
    keyName = keyName,
    returnItems = c('yellowcards', 'possesiontime')
  )

  expect_equal( result %>% length, 2 )
  expect_equal( result %>% sort, c(0, 65) )

})


test_that("Check that a list of commentary data can be aggregated correctly", {

  commentaryKeys <- paste0('cmt_commentary:', KEYS$COMP, '*') %>%
    rredis::redisKeys() %>%
    as.character

  ongoalVec <- c()
  for (i in 1:length(commentaryKeys)) {
    ongoalVec %<>% c(
      rredis::redisHMGet(
        key = commentaryKeys[i],
        fields = 'shots_ongoal'
      )
    )
  }
  ongoalVec %<>% as.integer

  averaged <- footballstats::commentary_stats(
    commentary = commentaryKeys,
    returnItems = c('shots_ongoal', 'saves')
  )

  expect_equal( averaged %>% length, 2 )
  expect_equal( averaged %>% sort, c(5.5, 6.5) )

  # Put two dummy keys into Redis to calculate average better
  addedOngoal <- c(10, 14)
  for (i in 1:length(addedOngoal)) {
    rredis::redisHSet(
      key = paste0('cmt_commentary:', KEYS$COMP, ':1:', i),
      field = 'shots_ongoal',
      value = addedOngoal[i]
    )
  }

  newKeys <- paste0('cmt_commentary:', KEYS$COMP, '*') %>%
    rredis::redisKeys() %>%
    as.character
  keyLen <- newKeys %>% length

  expect_equal( keyLen, 4 )

  averaged <- footballstats::commentary_stats(
    commentary = newKeys,
    returnItems = 'shots_ongoal'
  )
  totalStats <- c(ongoalVec, addedOngoal)

  expect_equal( averaged %>% length, 1 )
  expect_equal( averaged %>% round(digits = 2), totalStats %>% sum %>% `/`(keyLen) %>% round(digits = 2) )

})

test_that("Scale something at random", {

  # Set up test data set in the form that the features will take
  testData <- data.frame(
    one = c(2, 4, 6),
    two = c(-2, -10, -7),
    label = c('W', 'L', 'D'),
    stringsAsFactors = FALSE
  )

  # Get the scales based on min and max values of the data
  result <- testData %>% footballstats::get_scales()

  expect_that( result, is_a('list') )
  expect_equal( result$sMax %>% length, 2 )
  expect_equal( result$sMin %>% length, 2 )
  expect_equal( result$cols, 2 )

  # Scale the testData with the resultant scalers
  testScaled <- footballstats::scale_data(
    mDat = testData,
    dataScales = result
  )

  expect_that( testScaled, is_a('data.frame') )
  expect_equal( testScaled %>% nrow, 3 )
  expect_equal( testScaled %>% ncol, 2 )

})
