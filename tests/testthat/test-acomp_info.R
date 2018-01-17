context("test-acomp_info.R")

rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()

test_that("Save competition IDs from /competitions/ into a set", {

  res <- 17476L %>%
    as.Date(origin = '1970-01-01') %>%
    footballstats::format_dates()

  newCompetitions <- KEYS %>% footballstats::acomp_info()

  expect_that( newCompetitions, is_a('data.frame') )
  expect_that( newCompetitions %>% nrow, equals(22) )
  expect_that( newCompetitions %>% names, equals(c('id', 'name', 'region')) )

  uniqueComps <- 'competition:set' %>% rredis::redisSMembers()

  sortedComps <- uniqueComps %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  sortedData <- newCompetitions$id %>%
    as.integer %>%
    sort

  expect_that( sortedData, equals(sortedComps) )

})
