#' @title Add Competition Info
#'
#' @description A function that checks a unique competition has been 
#'  obtained and is added to a hashMap of the form ....
#'  -> competitions:set 
#'  
#' @param production A boolean to indicate whether production (default)
#'  runs are performed or testing carried out.
#' @param seasonIDs A list containing seasonIDs...
#'  
#' @return returns nothing, a redis hash is set with season IDs, and a 
#'  redis set is created to store the current seasonIDs.
#'  


addCompetitionInfo <- function(production = TRUE, daysUntilNextQuery) {
  if (production) {
    competitionIDs <- getCompetition(apiKey = API_KEY)
  }
  
  if (is.null(competitionIDs)) {
    return()
  } else {
    for (i in 1:nrow(competitionIDs)) {
      seasonID <- competitionIDs$id[[i]]
      seasonName <- competitionIDs$name[[i]]
      competitionInSet <- rredis::redisSIsMember(set = "competition:set", element = seasonID)
      if (!competitionInSet) {
        rredis::redisSAdd(set = "competition:set", element = charToRaw(seasonID))
        myCompetitionID <- getNextID(IDLookup = "competition")
        newID <- mapAPIToMyID(APIID = seasonID,
                              myID = myCompetitionID,
                              type = "competition")
      }
    }
    rredis::redisSet(key = "competition:waitForNextQuery", 
                     value = as.integer(Sys.Date() + daysUntilNextQuery))
  }
}
