
# Load all functions into global namespace
source(paste0(getwd(), '/R/UtilityFunctions/initialize.R'))
initialize(location = paste0(getwd(), '/R/'),
           redisHost = 'localhost',
           redisPort = 6379,
           testing = TRUE)
results <- test_dir(path = "tests", 
                    reporter = "summary")

rredis::redisSelect(1)

totalError <- 0
for(i in 1:length(results)) {
  singleTest <- results[i][[1]]
  testResults <- singleTest$results
  failures <- 0
  
  print(singleTest)
  print(testResults)
  print(testResults[[1]])
  for (j in 1:length(testResults)) {
    check <- testResults[[j]]

    print(check)
    print(check != "As expected ")
    if (check != "As expected ") {
      failures <- failures + 1
    }
  }

  fileName <- singleTest$file
  unitTest <- singleTest$test

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

quit(save = 'no',
     status = codeStatus, 
     runLast = FALSE)

 
