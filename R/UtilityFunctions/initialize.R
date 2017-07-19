initialize <- function(location = '/R/') {
  # Begin by sourcing all the package functions
  allFiles <- list.files(path = location,
                         pattern = "\\.[RrSsQq]$",
                         recursive = TRUE)
  allFilesLong <- paste0(location, setdiff(x = allFiles, 
                                           y = "daemonProcess.R"))
  capture.output <- sapply(allFilesLong, source, .GlobalEnv)
  
  # Initalize all libraries and global variables
  initializeAllFunctions()
}