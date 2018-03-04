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
KEYS <<- list(
  COMP= 1204,
  SEASON = 2017,
  DATE_FROM = NULL,
  DATE_TO = NULL,
  SLACK_PRNT = FALSE,
  TEST = TRUE,
  LOG_PRED = FALSE,
  LOGGING = FALSE
)

# Run the tests!
results <- testthat::test_dir(
  path = getwd() %>% paste0("/tests/testthat"),
  reporter = "summary"
)

