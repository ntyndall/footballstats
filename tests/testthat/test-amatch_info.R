context("test-amatch_info.R")

# Reset DB
KEYS$RED$FLUSHDB()

test_that("Check the keys are as they should be by adding match data", {

  # Add match data from footballstats data source *controlled by KEYS$TEST*
  newMatchData <- KEYS %>%
    footballstats::amatch_info()

  # Check redis for expected output
  matchIDs <- "csm:*" %>% KEYS$RED$KEYS() %>%
    purrr::flatten_chr() %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  expect_equal( newMatchData$id %>% as.integer %>% sort, matchIDs )

})
