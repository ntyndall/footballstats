context("test-acomp_info.R")

rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()

test_that("Save competition IDs from /competitions/ into a set", {

  newDate <- as.Date(x = 17476, origin = '1970-01-01')
  res <- format_dates(standardDateFormat = newDate)

  newCompetitions <- footballstats::acomp_info(
    KEYS = NULL)

  expect_that( newCompetitions, is_a('data.frame') )
  expect_that( nrow(newCompetitions), equals(22) )
  expect_that( newCompetitions %>% names(), equals(c('id', 'name', 'region')) )

  uniqueComps <- rredis::redisSMembers(
    set = 'competition:set')

  sortedComps <- uniqueComps %>%
    purrr::flatten_chr() %>%
    as.integer() %>%
    sort()

  sortedData <- newCompetitions$id %>%
    as.integer() %>%
    sort()

  expect_that( sortedData, equals(sortedComps) )

})
