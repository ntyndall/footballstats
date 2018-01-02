context("test-utils.R")

rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()

test_that('test the request limitations can be implemented', {

  allowedRequests <- 200
  timePeriod <- 10

  footballstats::request_limit(
    requestsAllowed = allowedRequests,
    timePeriod = timePeriod)

  timeout <- 'requestLimit' %>%
    rredis::redisTTL() %>%
    as.character %>%
    as.integer

  expect_gt(timeout, (timePeriod/2))

  # Remove the key to disable the expiry
  'requestLimit' %>% rredis::redisDelete()

  rredis::redisSet(
    key = 'requestLimit',
    value = 200 %>% as.character %>% charToRaw())

  footballstats::request_limit(
    requestsAllowed = 210,
    timePeriod = 1)

  expect_that( rredis::redisGet(key = 'requestLimit') %>% as.character %>% as.integer, equals(0) )

})


test_that("Convert a simple date to one used by the API", {

  newDate <- as.Date(x = 17476, origin = '1970-01-01')
  res <- footballstats::format_dates(standardDateFormat = newDate)

  components <- strsplit(x = res, split = '[.]')[[1]]

  expect_that( components[1], equals('06') )
  expect_that( components[2], equals('11') )
  expect_that( components[3], equals('2017') )

})


test_that("The season starting variable is returned", {

  year <- Sys.Date() %>% format('%Y') %>% as.integer
  month <- Sys.Date() %>% format('%m') %>% as.integer

  starting <- footballstats::start_season()
  year <- if (month < 7) year - 1 else year

  expect_that( starting, equals(year) )

})
