context("test-classify_utils.R")

rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()
competitionID <- 1204

test_that("Test that match data can be recreated easily.", {

  # Test no data returns a null data frame
  recreated <- footballstats::recreate_matchdata(
    competitionID = competitionID,
    seasonStarting = 2017,
    matchLimit = 1000)

  expect_that( recreated %>% nrow, equals(0) )

  # Put the test data into Redis
  matchData <- footballstats::amatch_info(
    competitionID = competitionID,
    dateFrom = NULL,
    dateTo = NULL,
    seasonStarting = 2017,
    KEYS = NULL,
    bypass = bypass)

  recreated <- footballstats::recreate_matchdata(
    competitionID = competitionID,
    seasonStarting = 2017,
    matchLimit = 1000)

  expect_that( matchData %>% nrow, equals(recreated %>% nrow) )
  expect_that( recreated %>% names, equals(matchData %>% names %>% setdiff('events')) )
  expect_that( matchData$id %>% as.integer %>% sort, equals(recreated$id %>% as.integer %>% sort) )

})


test_that("Test that commentary data is sent to Redis.", {

  # Recreate the match data that is in redis
  recreated <- footballstats::recreate_matchdata(
    competitionID = competitionID,
    seasonStarting = 2017,
    matchLimit = 1000)

  # Choose the right match ID to analyse
  recreated <- recreated[recreated$id == '2212950', ]

  footballstats::acommentary_info(
    competitionID = competitionID,
    matchIDs = recreated$id,
    localteam = recreated$localteam_id,
    visitorteam = recreated$visitorteam_id,
    KEYS = NULL,
    bypass = TRUE)

  commentaryKeys <- as.character(rredis::redisKeys(
    pattern = paste0('cmt_commentary:', competitionID, '*')))

  # Produces a list of available commentary names, Must be a total intersection
  commentaryNames <- footballstats::available_commentaries(
    commentaryKeys = commentaryKeys)

  expect_that( commentaryNames %>% length(), equals(9) )
  expect_that( 'shots_total' %in% commentaryNames, is_true() )
  expect_that( 'shots_ongoal' %in% commentaryNames, is_true() )
  expect_that( 'fouls' %in% commentaryNames, is_true() )
  expect_that( 'corners' %in% commentaryNames, is_true() )
  expect_that( 'offsides' %in% commentaryNames, is_true() )
  expect_that( 'possesiontime' %in% commentaryNames, is_true() )
  expect_that( 'yellowcards' %in% commentaryNames, is_true() )
  expect_that( 'redcards' %in% commentaryNames, is_true() )
  expect_that( 'saves' %in% commentaryNames, is_true() )

})

test_that("Check that the commentaries can be retrieved from redis as a double vector", {

  commentaryKeys <- as.character(rredis::redisKeys(
    pattern = paste0('cmt_commentary:', competitionID, '*')))

  # Make sure to pick the right commentary for testing
  teamIDs <- commentaryKeys %>%
    strsplit(split = '[:]') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer() %>%
    sort()

  keyName <- commentaryKeys[grepl(teamIDs[1], commentaryKeys) %>% which()]
  result <- footballstats::commentary_from_redis(
    keyName = keyName,
    returnItems = 'saves')

  expect_that( result %>% length(), equals(1) )
  expect_that( result, equals(1) )

  result <- footballstats::commentary_from_redis(
    keyName = keyName,
    returnItems = c('yellowcards', 'possesiontime'))

  expect_that( result %>% length(), equals(2) )
  expect_that( result %>% sort(), equals(c(0, 65)) )

})


test_that("Check that a list of commentary data can be aggregated correctly", {

  commentaryKeys <- as.character(rredis::redisKeys(
    pattern = paste0('cmt_commentary:', competitionID, '*')))

  ongoalVec <- c()
  for (i in 1:length(commentaryKeys)) {
    res <- rredis::redisHMGet(key = commentaryKeys[i], fields = 'shots_ongoal')
    ongoalVec <- c(ongoalVec, res)
  }
  ongoalVec <- ongoalVec %>% as.integer()

  averaged <- footballstats::commentary_stats(
    commentary = commentaryKeys,
    returnItems = c('shots_ongoal', 'saves'))

  expect_that( averaged %>% length(), equals(2) )
  expect_that( averaged %>% sort(), equals(c(5.5, 6.5)) )

  # Put two dummy keys into Redis to calculate average better
  addedOngoal <- c(10, 14)
  for (i in 1:length(addedOngoal)) {
  rredis::redisHSet(
    key = paste0('cmt_commentary:', competitionID, ':1:', i),
    field = 'shots_ongoal',
    value = addedOngoal[i])
  }

  newKeys <- as.character(rredis::redisKeys(
    pattern = paste0('cmt_commentary:', competitionID, '*')))
  keyLen <- newKeys %>% length()

  expect_that( keyLen, equals(4) )

  averaged <- footballstats::commentary_stats(
    commentary = newKeys,
    returnItems = 'shots_ongoal')
  totalStats <- c(ongoalVec, addedOngoal)

  expect_that( averaged %>% length(), equals(1) )
  expect_that( averaged %>% round(digits = 2), equals((totalStats %>% sum)/keyLen %>% round(digits = 2)) )

})
