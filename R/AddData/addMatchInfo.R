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
#' @param matches A dataframe containing a particular match with relevant
#'  information to be stored in a redis hash.
#' @param competition An integer containing the competition ID that the 
#'  teams and match information belong to.
#' @param nextMatch An integer value with the match ID as an identifier 
#'  to the current match details.
#'  
#' @return Returns nothing, a redis hash map is set with the match information
#'  and a check to see if the teams have been analysed before is carried out.
#'


addMatchInfo <- function(competitionID, dateFrom, dateTo) {
  valuesToRetain <- c("id", "comp_id", "formatted_date", "season",           
                      "week", "venue", "venue_id", "venue_city",     
                      "status", "timer", "time", "localteam_id",  
                      "localteam_name", "localteam_score", "visitorteam_id",
                      "visitorteam_name", "visitorteam_score", "ht_score",
                      "ft_score", "et_score", "penalty_local", "penalty_visitor")
  
  if (redis$EXISTS(key = 'active') == 0) {
    matches <- getMatches(competitionID = competitionID,
                          dateFrom = dateFrom,
                          dateTo = dateTo)
    checkRequestLimit()
  } else {
    print(Sys.time(), ' : Run out of requests in addMatchInfo()')
    matches <- NULL
  }
  
  if (!is.null(matches)) {
    for (i in 1:nrow(matches)) {
      single <- matches[i, ]
      matchItems <- single[ ,valuesToRetain] 
      
      # Check if team has been added to the set for analysis later.
      teamInSet <- redis$SADD(key = paste0('comp:_teams_:', competitionID),
                              member = matchItems$localteam_id)
      if (teamInSet == 1) {
        redis$LPUSH(key = 'analyseTeams', value = matchItems$localteam_id)
      } else {
        redis$LPUSH(key = 'updateTeams', value = matchItems$localteam_id)
      }
    
      # Check if match belongs to set
      matchInSet <- redis$SADD(key = paste0('comp:_matches_:', competitionID),
                              member = matchItems$id)
      
      if (matchInSet == 1) {
        matchKey <- paste0("comp:season:match:", matchItems$comp_id, ":", 
                           matchItems$season, ":", matchItems$id)
        redis$HMSET(key = matchKey, field = names(matchItems), 
                    value = as.character(matchItems))
      }
    }
    return(matches)
  } else {
    return(data.frame())
  }
}
