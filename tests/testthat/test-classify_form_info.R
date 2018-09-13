context('test-classify_form_info.R')

# Reset DB
KEYS$RED$FLUSHDB()

test_that('Calculate score based on vector of forms', {

  expect_equal( 'LLL' %>% footballstats::form_to_int(), 0 )
  expect_equal( 'DDD' %>% footballstats::form_to_int(), 3 )
  expect_equal( 'WWW' %>% footballstats::form_to_int(), 6 )
  expect_equal( 'WDL' %>% footballstats::form_to_int(), 3 )
  expect_equal( 'LDW' %>% footballstats::form_to_int(), 3 )
  expect_equal( 'W' %>% footballstats::form_to_int(), 2 )
  expect_equal( 'D' %>% footballstats::form_to_int(), 1 )
  expect_equal( 'L' %>% footballstats::form_to_int(), 0 )

})


test_that("Does the list return a character result of 'W' / 'L' / 'D' ", {

  subFunc <- function(c, o) {
    footballstats::match_result(
      scoreCurrent = c,
      scoreOther = o
    )
  }

  expect_equal( subFunc(c = 2, o = 1), 'W' )
  expect_equal( subFunc(c = 1, o = 1), 'D' )
  expect_equal( subFunc(c = 0, o = 1), 'L' )

})
