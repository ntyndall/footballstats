#' @title Add Commentary Info
#'
#' @description A function that takes a competitionID and matchID's, and
#'  determines general match statistics for both local team and visitor 
#'  team
#'  
#' @details EventID's are checked if they have been analysed already;
#'     ->   [c_eventInSet]:{comp_id}   ->   [SET]   
#'  The actual event information is stored as a hash map as; 
#'     ->   [cme]:{comp_id}:{match_id}:{event_id}  ->   [HASH]
#'  
#' @param competitionID An integer defining the competitionID that the
#'  team belongs to.
#' @param matchIDs An integer character vector of matchIDs that match
#'  the matchEvents.
#'  
#' @return Returns nothing, a redis hash map is set with the 
#'  commentary information and IDs are stored as a redis set.
#'


addCommentaryInfo <- function(competitionID, matchIDs, localteam, visitorteam) {
  
  for (i in 1:length(matchIDs)) {
    if (redisConnection$EXISTS(key = 'active') == 0) {
      commentary <- getCommentaryInfo(matchID = matchIDs[i])
      checkRequestLimit()
    } else {
      print(Sys.time(), ' : Run out of requests in addCommentaryInfo()')
      commentary <- NULL
    }

    teamIDs <- c(localteam[i], visitorteam[i])
    if (!is.null(commentary)) {
      teamStats <- commentary$match_stats
      if (length(teamStats) == 2) {
        local <- teamStats[['localteam']]
        if (!is.null(local)) {
          analyseLocal <- TRUE
          redisConnection$HMSET(key = paste0("cmt_commentary:", competitionID, ":", matchIDs[i], ":", localteam[i]),
                                field = names(local), 
                                value = as.character(local))
        } else {
          analyseLocal <- FALSE
        }
        
        visitor <- teamStats[['visitorteam']]
        if (!is.null(visitor)) {
          analyseVisitor <- TRUE
          redisConnection$HMSET(key = paste0("cmt_commentary:", competitionID, ":", matchIDs[i], ":", visitorteam[i]),
                                field = names(visitor), 
                                value = as.character(visitor))
        } else {
         analyseVisitor <- FALSE 
        }
      }
      
      playerStatsLocal <- commentary$player_stats$localteam$player %>% purrr::when(is.null(.) ~ data.frame(), ~ .)
      localNames <- names(playerStatsLocal)
      if (nrow(playerStatsLocal) > 0 && analyseLocal) {
        for (j in 1:nrow(playerStatsLocal)) {
          redisConnection$HMSET(key = paste0("cmp:", competitionID, ":", matchIDs[i], ":", playerStatsLocal[j, ]$id),
                                field = localNames, 
                                value = as.character(playerStatsLocal[j, ]))
        }
      }
      
      playerStatsVisitor <- commentary$player_stats$visitorteam$player %>% purrr::when(is.null(.) ~ data.frame(), ~ .)
      visitorNames <- names(playerStatsVisitor)
      if (nrow(playerStatsVisitor) > 0 && analyseVisitor) {
        for (j in 1:nrow(playerStatsVisitor)) {
          redisConnection$HMSET(key = paste0("cmp:", competitionID, ":", matchIDs[i], ":", playerStatsVisitor[j, ]$id),
                                field = visitorNames, 
                                value = as.character(playerStatsVisitor[j, ]))
        }
      }
    }
  }
}
