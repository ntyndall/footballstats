context('test-classify_form_info.R')

test_that('Calculate score based on vector of forms', {

  expect_that( footballstats::form_to_int(oldForms = 'LLL'), equals(0) )
  expect_that( footballstats::form_to_int(oldForms = 'DDD'), equals(3) )
  expect_that( footballstats::form_to_int(oldForms = 'WWW'), equals(6) )
  expect_that( footballstats::form_to_int(oldForms = 'WDL'), equals(3) )
  expect_that( footballstats::form_to_int(oldForms = 'LDW'), equals(3) )
  expect_that( footballstats::form_to_int(oldForms = 'W'), equals(2) )
  expect_that( footballstats::form_to_int(oldForms = 'D'), equals(1) )
  expect_that( footballstats::form_to_int(oldForms = 'L'), equals(0) )

})
