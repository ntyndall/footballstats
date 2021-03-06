library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(testthat, warn.conflicts = FALSE, quietly = TRUE)
library(footballstats, warn.conflicts = FALSE, quietly = TRUE)
library(covr, warn.conflicts = FALSE, quietly = TRUE)
library(purrr, warn.conflicts = FALSE, quietly = TRUE)
library(utils, warn.conflicts = FALSE, quietly = TRUE)
library(redux, warn.conflicts = FALSE, quietly = TRUE)

# Set up enough keys for testing
KEYS <<- footballstats::keys_for_testing()

# Run the tests
results <- testthat::test_dir(
  path = "testthat",
  reporter = "summary"
)

# Remove keys for good measure
KEYS$RED$FLUSHDB()

totalError <- 0
for(i in 1:length(results)) {
  singleTest <- results[i][[1]]
  testResults <- singleTest$results
  failures <- 0

  for (j in 1:length(testResults)) {
    check <- utils::capture.output(testResults[[j]])
    if (check[1] != "As expected ") failures %<>% `+`(1)
  }

  fileName <- singleTest$file[1]
  unitTest <- singleTest$test[1]

  if (failures > 0) {
    totalError %<>% `+`(1)
    cat(paste0(Sys.time(), " : Failure in { ", fileName, " } --> ( ", unitTest, " ) \n"))
  } else {
    cat(paste0(Sys.time(), " : Success in { ", fileName, " } --> ( ", unitTest, " ) \n"))
  }
}

# Calculate code status from running tests
codeStatus <- ifelse(
  test = totalError > 0,
  yes = 1,
  no = 0
)

# If successful, quit gracefully
quit(
  save = 'no',
  status = codeStatus,
  runLast = FALSE
)
