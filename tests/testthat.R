print('Inside testthat.R')

library(redux)

print('attempt to use installed version of redux..')

myR <- redux::hiredis()
myR$SET(key = 'test', value = 'one')
result <- myR$GET(key = 'test')

print('The result is')
print(result)

library(magrittr)
library(testthat)
library(footballstats)
library(covr)
library(purrr)
library(utils)
library(rredis)

#rredis::redisClose()

# Connect to DB 3 (away from production)
rredis::redisConnect(
  host = 'localhost',
  port = 6379,
  nodelay = FALSE
)
rredis::redisSelect(3)
rredis::redisFlushDB()

# Set up enough keys for testing
KEYS <<- footballstats::keys_for_testing()

results <- testthat::test_dir(
  path = "testthat",
  reporter = "summary"
)

# Remove keys for good measure
rredis::redisFlushDB()

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
