context("test-amatch_info.R")

# Reset DB
rredis::redisFlushDB()

test_that("Check the keys are as they should be by adding match data", {

  KEYS$DATE_FROM <- KEYS$DATE_TO <- NULL

  newMatchData <- footballstats::amatch_info(
    competitionID = competitionID,
    seasonStarting = seasonStarting,
    KEYS = KEYS
  )

  # Check redis for expected output
  matchIDs <- rredis::redisKeys(
    pattern = 'csm:*') %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  expect_equal( matchData$id %>% as.integer %>% sort, matchIDs )

})
