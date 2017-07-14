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

daemonProcess <- function(i = 0) {
  
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
  tryCatch({
      rredis::redisConnect('localhost', port = 6379)
    }, finally = print(paste0(Sys.time(), " : No Redis database found. Connect and try again.")))
  
  # Begin constant loop for polling data
  while (i == 0) {
    break
  }
  
}
