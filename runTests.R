
print("check directory to source functionality...")
print(getwd())

print("just about to run tests")
results <- test_dir("tests", reporter="summary")

totalError <- 0
for(i in 1:length(results)) {
  singleTest <- results[i][[1]]$results
  failures <- 0
  
  for (j in 1:length(singleTest)) {
    check <- singleTest[[j]]$message

    if (check != "As expected ") {
      failures <- failures + 1
    }
  }

  fileName <-results[i][[1]]$file
  unitTest <- results[i][[1]]$test
  
  if (failures > 0) {
    totalError <- totalError + 1
    cat(paste0(Sys.time(), " : Failure in { ", fileName, " } --> ( ", unitTest, " ) \n"))
  } else {
    cat(paste0(Sys.time(), " : Success in { ", fileName, " } --> ( ", unitTest, " ) \n"))
  }
  
  print("after running tests")
  print(results)
}

print(paste0("RESULTS ->> ", totalError))
print("_________________________________")

# Final value that is returned from running the script
totalError


                