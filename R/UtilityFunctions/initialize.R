initialize <- function(location, redisHost, redisPort,
                       db, testing = FALSE) {
  # Begin by sourcing all the package functions
  allFiles <- list.files(path = location,
                         pattern = "\\.[RrSsQq]$",
                         recursive = TRUE)
  allFiles <- setdiff(x = allFiles, y = c("UtilityFunctions/initialize.R",
                                          "MainFunctions/populateData.R"))
  sapply(paste0(location, allFiles), source, .GlobalEnv)

  
  # Initalize all libraries and global variables
  initializeAllFunctions(redisHost = redisHost,
                         redisPort = redisPort,
                         db = db,
                         testing = testing)
}
