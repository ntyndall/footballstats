context("test-amatch_info.R")

test_that("Check the keys are as they should be by adding match data", {

  rredis::redisConnect(host = 'localhost', port = 6379)
  rredis::redisSelect(3)
  
  newMatchData <- footballstats::amatch_info(
    competitionID = 1204, 
    dateFrom = NULL, 
    dateTo = NULL, 
    seasonStarting = 2017, 
    updateData = FALSE,
    KEYS = NULL, 
    bypass = bypass)
    
  # Check redis for expected output
  matchIDs <- rredis::redisKeys(
    pattern = 'csm:*') %>%
      strsplit(split = ':') %>%
        purrr::map(4) %>%
          purrr::flatten_chr() %>%
            as.integer() %>%
              sort()
  
  expect_that( matchData$id %>% as.integer() %>% sort(), equals(matchIDs))

})
