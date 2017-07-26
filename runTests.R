
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
  singleTest <- results[i][[1]]$results
  failures <- 0
  
  print('singleTest')
  print(singleTest)
  for (j in 1:length(singleTest)) {
    check <- singleTest[[j]]$message

    print('check')
    print(check)
    if (check != "As expected ") {
      failures <- failures + 1
    }
  }

  fileName <-results[i][[1]]$file
  unitTest <- results[i][[1]]$test
  print('other..')
  print(fileName)
  print(unitTest)
  print(failures)  

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

print('codeStatus')
print(codeStatus)
quit(save = 'no',
     status = codeStatus, 
     runLast = FALSE)

 
