library(RcppRedis)
redis <- new(RcppRedis::Redis)

test_that("will this pass the test?", {

  redis$set('t', 1)
  res <- redis$get('t')
  expect_that( res, equals(1) )

})
