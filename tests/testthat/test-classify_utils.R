context("test-classify_utils.R")

# Reset DB
KEYS$RED$FLUSHDB()

test_that("Test that match data can be recreated easily.", {

  # Test no data returns a null data frame
  recreated <- KEYS %>%
    footballstats::recreate_matchdata()

  expect_equal( recreated %>% nrow, 0 )

  # Put the test data into Redis
  matchData <- KEYS %>%
    footballstats::amatch_info(footballstats::data.2017[1:10, ])

  recreated <- KEYS %>%
    footballstats::recreate_matchdata()

  expect_equal( matchData %>% nrow, recreated %>% nrow )
  expect_equal( recreated %>% names, matchData %>% names %>% setdiff('events') )
  expect_equal( matchData$id %>% as.integer %>% sort, recreated$id %>% as.integer %>% sort )

})


test_that("Test that commentary data is sent to Redis.", {

  # Recreate the match data that is in redis
  recreated <- KEYS %>%
    footballstats::recreate_matchdata()

  # Choose the right match ID to analyse
  recreated <- recreated[recreated$id == '2212950', ]

  KEYS %>% footballstats::acommentary_info(
    matchIDs = recreated$id,
    localteam = recreated$localteam_id,
    visitorteam = recreated$visitorteam_id
  )

  # Check what keys have been added
  commentaryKeys <- paste0('csmt_commentary:', KEYS$COMP, '*') %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr()

  expect_equal( commentaryKeys %>% length, 2 )

})

test_that("Check that the commentaries can be retrieved from redis as a double vector", {

  commentaryKeys <- paste0('csmt_commentary:', KEYS$COMP, '*') %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr()

  # Make sure to pick the right commentary for testing
  teamIDs <- commentaryKeys %>%
    strsplit(split = '[:]') %>%
    purrr::map(5) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  keyName <- commentaryKeys[grepl(teamIDs[1], commentaryKeys) %>% which]
  result <- footballstats::commentary_from_redis(
    KEYS = KEYS,
    keyName = keyName,
    returnItems = "saves"
  )

  expect_equal( result %>% length, 1 )
  expect_equal( result, 2 )

  result <- footballstats::commentary_from_redis(
    KEYS = KEYS,
    keyName = keyName,
    returnItems = c("yellowcards", "possesiontime")
  )

  expect_equal( result %>% length, 2 )
  expect_equal( result %>% sort, c(0, 58) )

})


test_that("Check that a list of commentary data can be aggregated correctly", {

  commentaryKeys <- paste0('csmt_commentary:', KEYS$COMP, '*') %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr()

  # Get values for shots on goal
  ongoalVec <- sapply(
    X = commentaryKeys,
    FUN = function(x) x %>% KEYS$RED$HMGET("shots_ongoal")
  ) %>%
    purrr::flatten_chr() %>%
    as.integer

  # Put two dummy keys into Redis to calculate average better
  addedOngoal <- c(10, 14)
  lapply(
    X = 1:(addedOngoal %>% length),
    FUN = function(x) {
      paste0('csmt_commentary:', KEYS$COMP, ":", KEYS$SEASON, ":1:", x) %>%
        KEYS$RED$HSET(
          field = "shots_ongoal",
          value = addedOngoal[x]
        )
    }
  )

  newKeys <- paste0('csmt_commentary:', KEYS$COMP, '*') %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr()

  expect_equal( newKeys %>% length, 4 )
  totalStats <- c(ongoalVec, addedOngoal)

})
