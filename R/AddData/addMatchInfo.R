#' @title Add Match Info
#'
#' @description A function that takes a competitionID and season year to query
#'  for all the matches in a particular season and saves new teams to a set for 
#'  later analysis.
#'  
#' @details Match information is stored in a hash map as;
#'     ->   comp:season:match:{comp_id}:{season}:{match_id}   ->   [HASH]
#'  The matches involved are first checked to see if they already exist
#'  in redis by checking the set;
#'     ->   comp:_matchSetInfo_:{comp_id}   ->   [SET]
#'  The teams involved in the match are checked to see if they are new,
#'  by checking the set in redis;
#'     ->   comp:_teamSetInfo_:{comp_id}   ->   [SET]
#'  
#' @param competitionID An integer containing the competitionID that the 
#'  teams and match information belong to.
#' @param dateFrom A POSIXct value converted to dd.mm.yyyy format which denotes
#'  the start date for querying the API.
#' @param dateTo A POSIXct value converted to dd.mm.yyyy format which denotes
#'  the end date for querying the API.
#' @param updateData A boolean that is set to TRUE if team data is to be analysed
#'  again, i.e. after a match. FALSE to ignore and only analyse new teams. Generally
#'  set to FALSE for first time run.
#'  
#' @return Returns a match dataframe containing all match information to update 
#'  events in a particular match. Redis is updated with match information.
#' @return Returns a NULL dataframe if no matches are found.
#'


addMatchInfo <- function(competitionID, dateFrom, dateTo, updateData) {
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
  
  icount <- 0
  if (!is.null(matches)) {
    for (i in 1:nrow(matches)) {
      single <- matches[i, ]
      matchItems <- single[ ,valuesToRetain] 
      
      # Check if team has been added to the set for analysis later.
      # Or if it is ready to be updated after another match has been played.
      teamInSet <- redis$SADD(key = paste0('comp:_teamSetInfo_:'),
                              member = matchItems$localteam_id)

      if (teamInSet == 1 || updateData) {
        redis$LPUSH(key = 'analyseTeams', value = matchItems$localteam_id)
      }
    
      # Check if match belongs to set
      matchInSet <- redis$SADD(key = paste0('comp:_matchSetInfo_:', competitionID),
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
