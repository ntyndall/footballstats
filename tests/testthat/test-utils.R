context("test-utils.R")

# Reset DB
rredis::redisFlushDB()

test_that('test the request limitations can be implemented', {

  allowedRequests <- 200
  timePeriod <- 10

  footballstats::request_limit(
    requestsAllowed = allowedRequests,
    timePeriod = timePeriod
  )

  timeout <- 'requestLimit' %>%
    rredis::redisTTL() %>%
    as.character %>%
    as.integer

  expect_gt( timeout, timePeriod %>% `/`(2) )

  # Remove the key to disable the expiry
  'requestLimit' %>% rredis::redisDelete()

  rredis::redisSet(
    key = 'requestLimit',
    value = 200 %>% as.character %>% charToRaw()
  )

  footballstats::request_limit(
    requestsAllowed = 210,
    timePeriod = 1
  )

  expect_equal( rredis::redisGet(key = 'requestLimit') %>% as.character %>% as.integer, 0 )

})


test_that("Convert a simple date to one used by the API", {

  components <- 17476L %>%
    as.Date(origin = '1970-01-01') %>%
    footballstats::format_dates() %>%
    strsplit(split = '[.]') %>%
    purrr::flatten_chr()

  expect_equal( components[1], '06' )
  expect_equal( components[2], '11' )
  expect_equal( components[3], '2017' )

})


test_that("The season starting variable is returned", {

  year <- Sys.Date() %>% format('%Y') %>% as.integer
  month <- Sys.Date() %>% format('%m') %>% as.integer

  starting <- footballstats::start_season()
  year <- if (month < 7) year - 1 else year

  expect_equal( starting, year )

})
