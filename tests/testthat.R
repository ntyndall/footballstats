library(testthat)
library(footballstats)
library(covr)

print('get here')
#test_check("footballstats")

results <- test_dir(path = "testthat",
                    reporter = "summary")

rredis::redisConnect(host = 'localhost', port = 6379)
rredis::redisSelect(1)
#redisConnection <<- redisMapping()


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

if (totalError > 0) {
  codeStatus <- 1
} else {
  codeStatus <- 0
}


print('now test')
report()

quit(save = 'no',
     status = codeStatus,
     runLast = FALSE)


