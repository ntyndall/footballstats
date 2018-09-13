context('test-classify_form_info.R')

# Reset DB
KEYS$RED$FLUSHDB()

test_that('Calculate score based on vector of forms', {

  int_sum <- function(x) x %>% footballstats::form_to_int() %>% sum

  expect_equal( c("L", "L", "L") %>% int_sum(), 0 )
  expect_equal( c("D", "D", "D") %>% int_sum(), 3 )
  expect_equal( c("W", "W", "W") %>% int_sum(), 6 )
  expect_equal( c("W", "D", "L") %>% int_sum(), 3 )
  expect_equal( c("L", "D", "W") %>% int_sum(), 3 )
  expect_equal( "W" %>% int_sum(), 2 )
  expect_equal( "D" %>% int_sum(), 1 )
  expect_equal( "L" %>% int_sum(), 0 )

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
