library(testthat)
library(purrr)
library(magrittr)
library(footballstats)
library(rredis)

# Connect to DB 3 (away from production)
rredis::redisConnect(
  host = 'localhost',
  port = 6379,
  nodelay = FALSE
)
rredis::redisSelect(3)

# Set up enough keys for testing
KEYS <<- footballstats::keys_for_testing()

# Run the tests!
results <- testthat::test_dir(
  path = getwd() %>% paste0("/tests/testthat"),
  reporter = "summary"
)

