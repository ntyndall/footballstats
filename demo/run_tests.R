library(testthat, warn.conflicts = FALSE, quietly = TRUE)
library(purrr, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(footballstats, warn.conflicts = FALSE, quietly = TRUE)
library(redux, warn.conflicts = FALSE, quietly = TRUE)

# Set up enough keys for testing (db = 3)
KEYS <<- footballstats::keys_for_testing()

# Remove keys for good measure
KEYS$RED$FLUSHDB()

# Run the tests!
results <- testthat::test_dir(
  path = getwd() %>% paste0("/tests/testthat"),
  reporter = "summary"
)

# Remove keys for good measure (again)
KEYS$RED$FLUSHDB()
