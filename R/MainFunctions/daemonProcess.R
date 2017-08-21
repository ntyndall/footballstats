#' @title Daemon Process
#'
#' @description ...
#'  
#' @details ...
#'
#' @param None
#'  
#' @return Returns nothing.
#'
#'

daemonProcess <- function(i = 0, interval = 60 * 60) {
  
  # Begin by sourcing all the package functions
  location <- paste0("~", ROOT, "/R/")
  allFiles <- list.files(path = location,
                         pattern = "\\.[RrSsQq]$",
                         recursive = TRUE)
  allFilesLong <- paste0(location, setdiff(x = allFiles, 
                                           y = "daemonProcess.R"))
  capture.output <- sapply(allFilesLong, source, .GlobalEnv)
  
  # Initalize all libraries and global variables
  initializeAllFunctions()
  
  # Initiate the Redis connection if it exits.
  print(paste0(Sys.time(), " : Beginning Daemon..."))
  rredis::redisConnect('localhost', port = 6379)

  # Begin constant loop for polling data
  while (i == 0) {
    
    # Continuously check for new results (Go through usual flows..)
    for (i in 1:nrow(comps)) {
      matchesAnalysed <- daemonController(redisConnection = redisConnection,
                                          competitionID = comps[j],
                                          todaysDate = Sys.Date())
    }
    
    if (matchesAnalysed > 0) {
      
    }
    
    # Figure out when next matches are being played.
    
    
    # If any new events have been found then rebuild the models the day before a match.
    
    
    
    
    # Once complete then sleep for an hour and then retry again.
    Sys.sleep(interval)
    
    
    
    
    break
  }
  
}
