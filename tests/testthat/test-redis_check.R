context("test-redis_check.R")

# Reset DB
rredis::redisFlushDB()

test_that("Check redux is installed", {

  testCon$SET(key = 'test', value = 1)
  myVal <- testCon$GET(key = 'test')

  expect_equal( myVal, "1" )

})
