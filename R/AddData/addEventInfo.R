#' @title Add Event Info
#'
#' @description A function that takes a data frame with event information
#'  and stores relevant information in redis.
#'  
#' @details Event IDs are stored in a Redis set as 
#'  -> events:{nextEvent}
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


addEventInfo <- function(events, nextSingleEvent, nextEvent) {
    if (events$team == 'localteam') { 
      team <- "home" 
    } else { 
      team <- "away" 
    }

    eventList = list(originalID = events$id,
                     type = events$type,
                     minute = events$minute,
                     team = team,
                     player = events$player,
                     player_id = events$player,
                     assist = events$assist,
                     assist_id = events$assist_id)

    rredis::redisSAdd(set = paste0("events:", nextEvent), element = charToRaw(as.character(nextSingleEvent)))
    rredis::redisHMSet(key = paste0("singleEvent:", nextSingleEvent),
                       values = eventList)
}
