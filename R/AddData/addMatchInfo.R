#' @title Add Match Info
#'
#' @description A function that takes a data frame with a single match
#'  and stores relevant information for a particular competition.
#'  
#' @details Match information is stored in a hash map as 
#'    ->   matches:{comp_id}:{season}:{next_match}
#'  The teams involved in the match are checked to see if they are new,
#'  by checking the set in redis
#'    ->   teams:{competition}
#'  
#' @param myData A dataframe containing a particular match with relevant
#'  information to be stored in a redis hash.
#' @param competition An integer containing the competition ID that the 
#'  teams and match information belong to.
#' @param nextMatch An integer value with the match ID as an identifier 
#'  to the current match details.
#'  
#' @return Returns nothing, a redis hash map is set with the match information
#'  and a check to see if the teams have been analysed before is carried out.
#'


addMatchInfo <- function(myData, competition, nextMatch) {

  myList = list(home = myData$localteam_id, 
                away = myData$visitorteam_id,
                homeScore = myData$localteam_score,
                awayScore = myData$visitorteam_score,
                date = myData$formatted_date,
                week = myData$week,
                events = charToRaw(as.character(nextEvent)), 
                originalID = myData$id)
  key <- paste0("matches:", myData$comp_id, ":", myData$season, ":", nextMatch)
  rredis::redisHMSet(key = key, values = myList)

  inSet <- rredis::redisSIsMember(set = paste0("teams:", competition), 
                                  element = charToRaw(myData$localteam_id))
  matchResults <- data.frame(inSet = inSet)
  eventFrame <- myData$events[[1]]
  matchResults$events <- eventFrame
  return(matchResults)
  
  }
