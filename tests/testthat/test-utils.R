context("test-utils.R")

rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()

test_that('test the request limitations can be implemented', {

expect_that(1, equals(1) )

})


test_that("Convert a simple date to one used by the API", {

  newDate <- as.Date(x = 17476, origin = '1970-01-01')
  res <- format_dates(standardDateFormat = newDate)

  components <- strsplit(x = res, split = '[.]')[[1]]

  expect_that( components[1], equals('06') )
  expect_that( components[2], equals('11') )
  expect_that( components[3], equals('2017') )

})

