#' @title add_commentary_info
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


add_commentary_info <- function(competitionID, matchIDs, localteam, visitorteam) {

  commentary_sub <- function(competitionID, matchID, teamInfo, teamStats, commentary) {
    redisConnection$HMSET(key = paste0("cmt_commentary:", competitionID, ":", matchID, ":", teamInfo),
                          field = names(teamStats), 
                          value = as.character(teamStats))
    playerStats <- commentary$player %>% purrr::when(is.null(.) ~ data.frame(), ~ .)
    if (nrow(playerStats) > 0) {
      for (j in 1:nrow(playerStats)) {
        redisConnection$HMSET(key = paste0("cmp:", competitionID, ":", matchID, ":", playerStats[j, ]$id),
                              field = names(playerStats), 
                              value = as.character(playerStats[j, ]))
      }
    }  
  }
  
  for (i in 1:length(matchIDs)) {
    if (redisConnection$EXISTS(key = 'active') == 0) {
      commentary <- get_data(endpoint = paste0("/commentaries/", matchIDs[i], "?"))
      check_request_limit()
    } else {
      print(Sys.time(), ' : Run out of requests in addCommentaryInfo()')
      commentary <- NULL
    }

    localAway <- c('localteam', 'visitorteam')
    teamIDs <- c(localteam[i], visitorteam[i])
    if (!is.null(commentary)) {
      teamStats <- commentary$match_stats
      if (length(teamStats) == 2) {
        for (j in 1:length(localAway)) {
          singleTeamStats <- teamStats[[localAway[j]]]
          if (!is.null(singleTeamStats)) {
            commentary_sub(competitionID = competitionID, 
                           matchID = matchIDs[i], 
                           teamInfo = teamIDs[j],
                           teamStats = singleTeamStats,
                           commentary = commentary$player_stats[[localAway[j]]])
          }
        }
      }
    }
  }
}
