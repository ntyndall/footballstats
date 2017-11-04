#' @title Initialize
#'
#' @description A function that determines all the .R files (also named
#'  as function names) in the current package and sources them into
#'  the global environment for use by the analytics
#'  
#' @param location A character string that defines the root location of 
#'  the main folders of all the functions.
#' @param redisHost A character string that defines the host name for 
#'  the redis connection (defaults should be 'localhost').
#' @param redisPort An integer value that defines the port number for
#'  the redis connection (defaults to 6379).
#' @param db An integer value that defines the database number where
#'  the analytics is to be stored (between 0 and 16 - defaults to 10).
#' @param testing A boolean that defines whether tests are to be carried
#'  out, if TRUE then an older `rredis` package is used (due to failures
#'  in building dependencies that `redux` somehow complains about).
#'  
#' @return Returns nothing. All functions and libraries are sourced and
#'  loaded.
#'


initialize <- function(location, redisHost, redisPort,
                       db = 10, testing = FALSE) {

  # Begin by looking for all files in the directory
  allFiles <- list.files(path = location,
                         pattern = "\\.[RrSsQq]$",
                         recursive = TRUE)
  
  # Exclude scripts and other files not to be sourced
  allFiles <- setdiff(x = allFiles, y = c("UtilityFunctions/initialize.R",
                                          "MainFunctions/populateData.R",
                                          "analyseData/controllerScript.R"))
  sapply(paste0(location, allFiles), source, .GlobalEnv)

  # Initalize all libraries and global variables
  initializeAllFunctions(redisHost = redisHost,
                         redisPort = redisPort,
                         db = db,
                         testing = testing)
}
