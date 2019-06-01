context("test-amatch_info.R")

# Reset DB
KEYS$RED$FLUSHDB()

test_that("Check the keys are as they should be by adding match data", {

  # Add match data from footballstats
  newMatchData <- KEYS %>%
    footballstats::amatch_info(footballstats::data.2017[1:10, ])

  # Check redis for expected output
  matchIDs <- "csm:*" %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr() %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  expect_equal( newMatchData$zzz.matchID %>% as.integer %>% sort, matchIDs )

})
