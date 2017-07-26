#' @title Add Event Info
#'
#' @description A function that takes a competitionID, matchID's, and 
#'  a data frame containing match event information to be split up and
#'  added to redis as single events.
#'  
#' @details EventID's are checked if they have been analysed already;
#'     ->    comp:_eventInSet_:{comp_id}   ->   [SET]   
#'The actual event information is stored as a hash map as; 
#'     ->   comp:match:event:_singleEvent_:
#'          {comp_id}:{match_id}:{event_id}   ->   [HASH]
#'  
#' @param competitionID An integer defining the competitionID that the
#'  team belongs to.
#' @param matchIDs An integer character vector of matchIDs that match
#'  the matchEvents.
#' @param matchEvents A list of data frames containing individual events
#'  grouped per match.
#'  
#' @return Returns nothing, a redis hash map is set with the event information
#'  and IDs are stored as a redis set.
#'


addEventInfo <- function(competitionID, matchIDs, matchEvents) {
  for (i in 1:length(matchEvents)) {
    eventsPerMatch <- matchEvents[[i]]
    matchID <- matchIDs[i]
    if (length(eventsPerMatch) > 0) {
      for (j in 1:nrow(eventsPerMatch)) {
        event <- eventsPerMatch[j, ]
        inSet <- redis$SADD(key = paste0("comp:_eventInSet_:", competitionID), 
                            member = event$id)
        if (inSet == 1) {
          redis$HMSET(key = paste0("comp:match:event:_singleEvent_", competitionID, ":", matchID, ":", event$id),
                      field = names(event), value = as.character(event))
        }
      }
    }
  }
}
