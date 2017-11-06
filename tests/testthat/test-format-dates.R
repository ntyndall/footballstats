context("test-format-dates.R")

test_that("Convert a simple date to one used by the API", {
 
  newDate <- as.Date(x = 17476, origin = '1970-01-01')
  res <- format_dates(standardDateFormat = newDate)

  components <- strsplit(x = res, split = '[.]')[[1]]

  expect_equal( components[1], equals('06') )
  expect_equal( components[2], equals('11') )
  expect_equal( components[3], equals('2017') )
})

