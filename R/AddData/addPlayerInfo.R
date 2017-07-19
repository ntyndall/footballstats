#' @title Add Player Info

addPlayerInfo <- function(competitionID, playerLength) {
  valuesToRetain <- c("id", "common_name", "name", "firstname",
                      "lastname", "team", "teamid", "nationality",
                      "birthdate", "age", "birthcountry",
                      "birthplace", "position", "height", "weight")
  
  for (i in 1:playerLength) {
    if (redis$EXISTS(key = 'active') == 0) {
      playerID <- redis$LPOP(key = 'analysePlayers')
      playerData <- getPlayers(playerID = playerID)
      checkRequestLimit()
    } else {
      print(Sys.time(), ' : Run out of requests in addPlayerInfo()')
      playerData <- NULL
    }

    if (!is.null(playerData)) {
      stats <- playerData$player_statistics
      statNames <- names(stats)
      for (j in 1:length(statNames)) {
        statData <- stats[[statNames[j]]]
        if (length(statData) != 0 || is.data.frame(statData)) {
          for (k in 1:nrow(statData)) {
            currentStat <- statData[k, ]
            statKeyName <- paste0('player:team:league:season:stats:_', statNames[j], '_:', 
                                  playerData$id, ':', currentStat$id, ':', 
                                  currentStat$league_id, ':', currentStat$season)
            redis$HMSET(key = statKeyName, field = names(currentStat),
                       value = as.character(currentStat))
          }
        }
      }
    }
  }
}
