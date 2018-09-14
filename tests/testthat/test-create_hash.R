context("test-create_hash")


test_that("Convert a simple hash return object from redis", {

  result <- list("key", "value") %>%
    footballstats::create_hash()

  expect_equal( result$key, "value" )

})

test_that("Convert a simple hash return object from redis", {

  # Set up a redis key name
  rKey <- "test"
  val <- "myvalue"

  # Push a raw value conversion, i.e. an integer to redis
  rKey %>% KEYS$RED$HSET(
    field = "key",
    value = redux::object_to_bin(val)
  )

  # Make sure the object going in is a raw value
  expect_is( redux::object_to_bin(val), "raw" )

  # Can the value come out deserialized?
  result <- rKey %>%
    KEYS$RED$HGETALL() %>%
    footballstats::create_hash()

  expect_equal( result$key, val )

})
