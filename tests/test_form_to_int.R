#' 
#' @details Test getNextID()
#'


# Begin testing  
test_that("first test", {
  
  expect_that(1, is_a("numeric"))
  expect_that(1, equals(1))
  
})

test_that("second test", {
  res <- classify_form_to_int(oldForms = 'WLD')
  
  expect_that(res, equals(3))
})