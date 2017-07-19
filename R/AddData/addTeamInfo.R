#' @title Add Team Info
#'
#' @description A function that takes a data frame containing team
#'  information and stores that hash within Redis
#'  
#' @details Hash keys are stored as teams:competition:nextTeam
#'  
#' @param teamData A dataframe containing all teams data to be stored
#'  in redis in a hash.
#' @param competition An integer defining the competition ID that the
#'  team belongs to.
#' @param nextTeam An integer value that contains the next team ID
#'  value mapped from the original value.
#'  
#' @return Returns nothing, a redis hash map is set with the team
#'  information. 
#'


addTeamInfo <- function(competitionID, teamListLength) {
  valuesToRetain <- c("team_id", "is_national", "name", "country",
                      "founded", "leagues", "venue_name", "venue_id",
                      "venue_surface", "venue_address", "venue_city",
                      "venue_capacity", "coach_name", "coach_id")
  
  for (i in 1:teamListLength) {
    if (redis$EXISTS(key = 'active') == 0) {
      teamID <- redis$LPOP(key = 'analyseTeams')
      teamData <- getTeams(teamID = teamID)
      checkRequestLimit()
    } else {
      print(Sys.time(), ' : Run out of requests in addTeamInfo()')
      teamData <- NULL
    }
  
    if(!is.null(teamData)) {
      basic <- paste0("comp:team:_basic_:", competitionID, ":", teamData$team_id)
      squad <- paste0("comp:team:player:_squad_:", competitionID, ":", teamData$team_id)
      stats <- paste0("comp:team:_stats_:", competitionID, ":", teamData$team_id)

      basicData <- teamData[valuesToRetain]
      redis$HMSET(key = basic, field = names(basicData), 
                  value = as.character(basicData))
      
      squadInfo <- teamData$squad
      for (k in 1:nrow(squadInfo)) {
        playerID <- squadInfo$id[k]
        squadPlayer <- paste0(squad, ":", playerID)
        redis$HMSET(key = squadPlayer, field = names(squadInfo[k, ]), 
                    value = as.character(squadInfo[k, ]))
        newPlayers <- redis$SADD(key = 'teams:squad:playerIDs', 
                                 member = playerID)
        if (newPlayers == 1) {
          redis$LPUSH(key = 'analysePlayers', value = playerID)
        }
      }
      redis$HMSET(key = stats, field = names(teamData$statistics), 
                  value = as.character(teamData$statistics))
    }
  }
}
