#' @title Add Event Info
#'
#' @description A function that takes a data frame with event information
#'  and stores relevant information in redis.
#'  
#' @details Event IDs are stored in a Redis set as 
#'  -> events:{matchID}
#'  The actual event information is stored as a hash map as
#'  -> singleEvent:{nextSingleEvent}
#'  
#' @param events A dataframe containing a particular event to be stored
#'  in redis as a hash.
#' @param nextSingleEvent An integer value that contains the next event
#'  ID.
#' @param nextEvent ...
#'  
#' @return Returns nothing, a redis hash map is set with the event information
#'  and IDs are stored as a redis set.
#'


addEventInfo <- function(competitionID, matchIDs, matchEvents) {
  for (i in 1:length(matchEvents)) {
    eventsPerMatch <- matchEvents[[i]]
    matchID <- matchIDs[i]
    if (nrow(eventsPerMatch) > 0) {
      for (j in 1:nrow(eventsPerMatch)) {
        event <- eventsPerMatch[j, ]
        inSet <- redis$SADD(key = paste0("comp:events:", competitionID, ":", matchID), 
                            member = event$id)
        if (inSet == 1) {
          redis$HMSET(key = paste0("comp:match:event:", competitionID, ":", matchID, ":", event$id),
                      field = names(event), value = as.character(event))
        }
      }
    }
  }
}
