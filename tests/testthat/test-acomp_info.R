context("test-acomp_info.R")

# Reset DB
KEYS$RED$FLUSHDB()

test_that("Save competition IDs from /competitions/ into a set", {

  res <- 17476L %>%
    as.Date(origin = '1970-01-01') %>%
    footballstats::format_dates()

  newCompetitions <- KEYS %>%
    footballstats::acomp_info()

  expect_is( newCompetitions, 'data.frame' )
  expect_equal( newCompetitions %>% nrow, 22 )
  expect_equal( newCompetitions %>% names, c('id', 'name', 'region') )

  uniqueComps <- 'competition:set' %>% KEYS$RED$SMEMBERS()

  sortedComps <- uniqueComps %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  sortedData <- newCompetitions$id %>%
    as.integer %>%
    sort

  expect_equal( sortedData, sortedComps )

})
