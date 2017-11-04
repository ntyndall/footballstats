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


addCompetitionInfo <- function() {
  if (redisConnection$EXISTS(key = 'active') == 0) {
    competitionIDs <- get_data(endpoint = "/competitions?",
                               apiKey = API_KEY)
    check_request_limit()
  } else {
    print(Sys.time(), ' : Run out of requests in addCompetitionInfo()')
  }

  if (!is.null(competitionIDs)) {
    total <- 0
    for (i in 1:nrow(competitionIDs)) {
      seasonID <- competitionIDs$id[[i]]
      compExists <- redisConnection$SADD(key = 'competition:set',
                                         member = seasonID)
      if (compExists == 1) {
        total <- total + 1
      }
    }
    print(paste0(Sys.time(), ' : Successfully added ', total, ' new competition IDs to Redis.'))
    #redisConnection$SET(key = 'competition:waitForNextQuery',
    #                    value = as.integer(Sys.Date() + daysUntilNextQuery))
    return(competitionIDs)
  }
}
