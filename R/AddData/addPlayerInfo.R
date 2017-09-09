#' @title Add Player Info
#'
#' @description A function that takes a competitionID and length of players to 
#'  analyse. The playerID's are popped from a Redis list and queried. The player
#'  stats are then stored in appropriate redis keys as necessary.
#'  
#' @details Player stats infromation is stored in a hash map as;
#'     ->   [ctps]:{comp_id}:{team_id}:{player_id}:{season}:[_stats_`statType`_]   ->   [HASH]
#'  
#' @param competitionID An integer containing the competitionID that the 
#'  teams and match information belong to.
#' @param playerLength An integer value that defines the number of players to
#'  analyse for a given list of ID's previously generated.
#'  
#' @return Returns nothing. Redis is updated with player information
#'


addPlayerInfo <- function(competitionID, playerLength, currentSeasonYear) {
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
            season <- substr(currentStat$season, 1, 4)
            
            if (nchar(season) == 4) {
              seasonInt <- as.integer(season)
            } else {
              seasonInt <- 0
            }
            
            if (seasonInt == currentSeasonYear) {
              statKeyName <- paste0('ctps_', statNames[j], ':', currentStat$league_id, ':',
                                    currentStat$id, ':', playerData$id, ':', season)
              redisConnection$HMSET(key = statKeyName, 
                                    field = names(currentStat),
                                    value = as.character(currentStat))
            }
          })
        }
      })
    }
    setTxtProgressBar(progressBar, i)
  })
  close(progressBar)
}
