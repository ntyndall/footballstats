context("test-amatch_info.R")

# Reset DB
rredis::redisFlushDB()

test_that("Check the keys are as they should be by adding match data", {

  newMatchData <- KEYS %>% footballstats::amatch_info( )

  # Check redis for expected output
  matchIDs <- rredis::redisKeys(
    pattern = 'csm:*') %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  expect_equal( newMatchData$id %>% as.integer %>% sort, matchIDs )

})
