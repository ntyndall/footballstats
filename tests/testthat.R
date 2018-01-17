library(testthat)
library(footballstats)
library(covr)
library(purrr)
library(rredis)

# Set global variable to bypass the API calls
bypass <<- TRUE

rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)

# Set up enough keys for testing
KEYS <<- list(
  SLACK_PRNT = FALSE,
  TEST = TRUE,
  LOG_PRED = FALSE)

results <- testthat::test_dir(
  path = "testthat",
  reporter = "summary")

totalError <- 0
for(i in 1:length(results)) {
  singleTest <- results[i][[1]]
  testResults <- singleTest$results
  failures <- 0

  for (j in 1:length(testResults)) {
    check <- capture.output(testResults[[j]])
    if (check[1] != "As expected ") {
      failures <- failures + 1
    }
  }

  fileName <- singleTest$file[1]
  unitTest <- singleTest$test[1]

  if (failures > 0) {
    totalError <- totalError + 1
    cat(paste0(Sys.time(), " : Failure in { ", fileName, " } --> ( ", unitTest, " ) \n"))
  } else {
    cat(paste0(Sys.time(), " : Success in { ", fileName, " } --> ( ", unitTest, " ) \n"))
  }
}

# Calculate code status from running tests
codeStatus <- ifelse(
  test = totalError > 0,
  yes = 1,
  no = 0)

# If successful, quit gracefully
quit(
  save = 'no',
  status = codeStatus,
  runLast = FALSE)


