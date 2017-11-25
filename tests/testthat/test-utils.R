context("test-utils.R")

rredis::redisFlushDB()

test_that('test the request limitations can be implemented', {

  allowedRequests <- 200
  timePeriod <- 10

  request_limit(
    requestsAllowed = allowedRequests,
    timePeriod = timePeriod)

  timeout <- rredis::redisTTL(key = 'requestLimit') %>%
    as.character() %>%
    as.integer()

  expect_gt(timeout, (timePeriod/2))

  # Remove the key to disable the expiry
  rredis::redisDelete(
    key = 'requestLimit')

  rredis::redisSet(
    key = 'requestLimit',
    value = 200 %>% as.character() %>% charToRaw())

  request_limit(
    requestsAllowed = 210,
    timePeriod = 1)

  expect_that( rredis::redisGet(key = 'requestLimit') %>% as.character() %>% as.integer(), equals(0) )

})


test_that("Convert a simple date to one used by the API", {

  newDate <- as.Date(x = 17476, origin = '1970-01-01')
  res <- format_dates(standardDateFormat = newDate)

  components <- strsplit(x = res, split = '[.]')[[1]]

  expect_that( components[1], equals('06') )
  expect_that( components[2], equals('11') )
  expect_that( components[3], equals('2017') )

})

