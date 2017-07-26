initialize <- function(location, redisHost, redisPort,
                       db = 10, testing = FALSE) {
  print('sourcing..')
  # Begin by sourcing all the package functions
  allFiles <- list.files(path = location,
                         pattern = "\\.[RrSsQq]$",
                         recursive = TRUE)
  allFiles <- setdiff(x = allFiles, y = c("UtilityFunctions/initialize.R",
                                          "MainFunctions/populateData.R"))
  sapply(paste0(location, allFiles), source, .GlobalEnv)

  print('before loading libraries')
  print(paste0('value of testing...', testing))
  # Initalize all libraries and global variables
  initializeAllFunctions(redisHost = redisHost,
                         redisPort = redisPort,
                         db = db,
                         testing = testing)
}
