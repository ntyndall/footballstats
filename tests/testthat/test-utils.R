context("test-utils.R")

# Reset DB
KEYS$RED$FLUSHDB()

test_that('test the request limitations can be implemented', {

  allowedRequests <- 200
  timePeriod <- 10

  KEYS %>% footballstats::request_limit(
    requestsAllowed = allowedRequests,
    timePeriod = timePeriod
  )

  timeout <- "requestLimit" %>%
    KEYS$RED$TTL()

  exceeded <- timePeriod %>% `/`(2)

  expect_gt( 4, 3 )
  expect_gt( timeout, exceeded )

  # Remove the key to disable the expiry
  "requestLimit" %>% KEYS$RED$DEL()
  "requestLimit" %>% KEYS$RED$SET(200)

  KEYS %>% footballstats::request_limit(
    requestsAllowed = 210,
    timePeriod = 1
  )

  expect_equal( KEYS$RED$GET(key = "requestLimit") %>% as.integer, 0 )

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
