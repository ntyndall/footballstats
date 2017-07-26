#' @title Store Data
#'
#' @description A function that loads all functionality and stores all relevant
#'  information to Redis
#'  
#' @details 
#'  (1) Load all functions
#'  (2) Import ALL data with autoAnalyse = FALSE
#'  (3) Update relevant data with autoAnalyse = TRUE
#'  (4) Set start seasonDate e.g. seasonStarting = 2015 for 2015/2016 season
#'  (5) Provide Redis database configured in (1)
#'  
#' @param competitionID An integer containing the competitionID that the 
#'  teams and match information belong to.
#' @param seasonStarting An integer defining the lower year for details on a season.
#' @param updateData A boolean to signify whether information is only to be looked at
#'  for the first time or whether data needs updated (i.e. a new match has been played.)
#'  


storeData <- function(competitionID, seasonStarting, updateData) {
  initialize(location = paste0(getwd(), '/Desktop/football-project/footballstats/R/'),
             redisHost = 'localhost',
             redisPort = 6379,
             db = 10)
  
  mainController(redisConnection = redisConnection,
                 competitionID = competitionID, 
                 updateData = updateData, 
                 seasonStarting = seasonStarting)
}
