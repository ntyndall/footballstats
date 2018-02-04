library(testthat)
library(purrr)
library(footballstats)
library(rredis)

# Connect to DB 3 (away from production)
rredis::redisConnect(
  host = 'localhost',
  port = 6379,
  nodelay = FALSE
)
rredis::redisSelect(3)

# Initialise used and common variables
competitionID <- 1204
seasonStarting <- 2017

# Set up enough keys for testing
KEYS <<- list(
  SLACK_PRNT = FALSE,
  TEST = TRUE,
  LOG_PRED = FALSE
)

# Run the tests!
results <- testthat::test_dir(
  path = getwd() %>% paste0("/tests/testthat"),
  reporter = "summary"
)

