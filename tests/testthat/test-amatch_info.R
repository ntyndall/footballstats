context("test-amatch_info.R")

# Reset DB
rredis::redisFlushDB()

test_that("Check the keys are as they should be by adding match data", {

  newMatchData <- footballstats::amatch_info(
    competitionID = competitionID,
    dateFrom = NULL,
    dateTo = NULL,
    seasonStarting = seasonStarting,
    KEYS = KEYS)

  # Check redis for expected output
  matchIDs <- rredis::redisKeys(
    pattern = 'csm:*') %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  expect_that( matchData$id %>% as.integer %>% sort, equals(matchIDs) )

})
