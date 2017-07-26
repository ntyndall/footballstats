#' @title Add Player Info
#'
#' @description A function that takes a competitionID and length of players to 
#'  analyse. The playerID's are popped from a Redis list and queried. The player
#'  stats are then stored in appropriate redis keys as necessary.
#'  
#' @details Player stats infromation is stored in a hash map as;
#'     ->   player:team:league:season:
#'          _stats_`stat_type`:{player_id}:{team_id}:
#'          {comp_id}:{season}   ->   [HASH]
#'  
#' @param competitionID An integer containing the competitionID that the 
#'  teams and match information belong to.
#' @param playerLength An integer value that defines the number of players to
#'  analyse for a given list of ID's previously generated.
#'  
#' @return Returns nothing. Redis is updated with player information
#'

addPlayerInfo <- function(competitionID, playerLength) {
  valuesToRetain <- c("id", "common_name", "name", "firstname",
                      "lastname", "team", "teamid", "nationality",
                      "birthdate", "age", "birthcountry",
                      "birthplace", "position", "height", "weight")
  
  progressBar <- txtProgressBar(min = 0, max = playerLength, style = 3)
  sapply(1:playerLength, function(i) {
    if (redisConnection$EXISTS(key = 'active') == 0) {
      playerID <- redisConnection$LPOP(key = 'analysePlayers')
      playerData <- getPlayers(playerID = playerID)
      checkRequestLimit()
    } else {
      print(Sys.time(), ' : Run out of requests in addPlayerInfo()')
      playerData <- NULL
    }

    if (!is.null(playerData)) {
      stats <- playerData$player_statistics
      statNames <- names(stats)
      sapply(1:length(statNames), function(j) {
        statData <- stats[[statNames[j]]]
        if (length(statData) != 0 || is.data.frame(statData)) {
          sapply(1:nrow(statData), function(k) {
            currentStat <- statData[k, ]
            statKeyName <- paste0('player:team:league:season:_stats_', statNames[j], '_:', 
                                  playerData$id, ':', currentStat$id, ':', 
                                  currentStat$league_id, ':', currentStat$season)
            redisConnection$HMSET(key = statKeyName, field = names(currentStat),
                                  value = as.character(currentStat))
          })
        }
      })
    }
    setTxtProgressBar(progressBar, i)
  })
  close(progressBar)
}
