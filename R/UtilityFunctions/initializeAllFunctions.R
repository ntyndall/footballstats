#' @title Initialize All Functions
#'
#' @description A function that calls all the initialization functions
#'  required for loading variables into the global namespace and also
#'  imports required libraries
#'  
#' @param redisHost A character string that defines the host name
#'  to create the redis connection. Default set to `localhost`
#' @param redisPort An integer value that defines the port number
#'  to create the redis connection. Default set to `6379`
#'  
#' @return returns nothing, libraries are loaded and variables 
#'  loaded into the global environment
#'


initializeAllFunctions <- function(redisHost = "localhost", redisPort = 6379, 
                                   db = 10, testing) {
  print('bef all libs')
  # Load all libraries first.
  library(ggplot2)
  library(slackr)
  library(jsonlite)
  library(testthat)
  library(tcltk)
  print('about to load...')
  if (testing) {
    print('rredis...')
    library(rredis)
    redisConnection <<- rredis::redisConnect(host = redisHost,
                                             port = redisPort)
  } else {
    print('redux..')
    library(redux)
    sensitiveInfo()
    getIDList()
    redisConnection <<- redux::hiredis(host = redisHost, 
                                       port = redisPort, 
                                       db = db)
  }
}
