#' @title add_match_info
#'
#' @description A function that takes a competitionID and season year to query
#'  for all the matches in a particular season and saves new teams to a set for 
#'  later analysis.
#'  
#' @details Match information is stored in a hash map as;
#'     ->   [csm]:{comp_id}:{season}:{match_id}   ->   [HASH]
#'  The matches involved are first checked to see if they already exist
#'  in redis by checking the set;
#'     ->   [c_matchSetInfo]:{comp_id}   ->   [SET]
#'  The teams involved in the match are checked to see if they are new,
#'  by checking the set in redis;
#'     ->   [c_teamSetInfo]:{comp_id}   ->   [SET]
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
#' @param analysingToday A boolean that is set to TRUE if data is being analysing today.
#'  This is used to figure out if matches have been played during the time of 
#'  query, if not then wait until todays match has been played.
#'  
#' @return Returns a match dataframe containing all match information to update 
#'  events in a particular match. Redis is updated with match information.
#' @return Returns a NULL dataframe if no matches are found.
#'


add_match_info <- function(competitionID, dateFrom, dateTo, seasonStarting, updateData, 
                         analysingToday = TRUE, KEYS) {
  valuesToRetain <- c("id", "comp_id", "formatted_date", "season",           
                      "week", "venue", "venue_id", "venue_city",     
                      "status", "timer", "time", "localteam_id",  
                      "localteam_name", "localteam_score", "visitorteam_id",
                      "visitorteam_name", "visitorteam_score", "ht_score",
                      "ft_score", "et_score", "penalty_local", "penalty_visitor")
  
  if (redisConnection$EXISTS(key = 'active') == 0) {
    matches <- get_data(endpoint = paste0("/matches?comp_id=", competitionID, "&from_date=", dateFrom,
                                          "&to_date=", dateTo, "&"),
                        KEYS = KEYS)
    check_request_limit()

    # If getting todays match information, make sure all matches have actually been played.
    if (analysingToday) {
       if (any(matches$localteam_score == "")) {
         return(data.frame(stringsAsFactors = FALSE))
       }
    }
  } else {
    print(Sys.time(), ' : Run out of requests in addMatchInfo()')
    matches <- NULL
  }

  if (!is.null(matches)) {
    for (i in 1:nrow(matches)) {
      single <- matches[i, ]
      matchItems <- single[ ,valuesToRetain] 
      
      # Check if team has been added to the set for analysis later.
      # Or if it is ready to be updated after another match has been played.
      teamInSet <- redisConnection$SADD(key = paste0('c_teamSetInfo:', competitionID),
                                        member = matchItems$localteam_id)

      if (teamInSet || updateData) {
        redisConnection$LPUSH(key = 'analyseTeams', 
                              value = matchItems$localteam_id)
      }
    
      # Check if match belongs to set
      matchInSet <- redisConnection$SADD(key = paste0('c_matchSetInfo:', competitionID),
                                         member = matchItems$id)
      
      if (matchInSet) {
        matchKey <- paste0("csm:", matchItems$comp_id, ":", 
                           seasonStarting, ":", matchItems$id)
        redisConnection$HMSET(key = matchKey, field = names(matchItems), 
                              value = as.character(matchItems))
        
        if (redisConnection$EXISTS(key = paste0('c:', competitionID, ':pred:', matchItems$id))) {
          redisConnection$SADD(key = paste0('c:', competitionID, ':ready'),
                               member = matchItems$id)
        }
      }
    }
    return(matches)
  } else {
    return(data.frame(stringsAsFactors = FALSE))
  }
}
