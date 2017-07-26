#' @title Add Team Info
#'
#' @description A function that takes a competitionID and integer value
#'  with details of the teamID list for analysis. Each team is
#'  queried by the API for relevant information and statistics are
#'  stored.
#'  
#' @details A number of information is stored. 
#'  The basic information is stored as;
#'     ->   comp:team:_basic_:{comp_id}:{team_id}   ->   [HASH]
#'  The team statistics is stored as;
#'     ->   comp:team:_stats_:{comp_id}:{team_id}   ->   [HASH]
#'  Player information relevent to the team is stored as;
#'     ->   comp:team:player:_squad_:
#'          {comp_id}:{team_id}:{player_id}   ->   [HASH]
#'
#' @param competitionID An integer defining the competitionID that the
#'  team belongs to.
#' @param teamListLength An integer value that defines how long the list 
#'  containing teamID's is TeamID's are then popped from the list as they 
#'  are anaylsed.
#' @param updateData A boolean that is set to TRUE if team data is to be analysed
#'  again, i.e. after a match. FALSE to ignore and only analyse new teams. Generally
#'  set to FALSE for first time run.
#'  
#' @return Returns nothing. A Redis hash map is set with the team
#'  information. 
#'


addTeamInfo <- function(competitionID, teamListLength, updateData) {
  valuesToRetain <- c("team_id", "is_national", "name", "country",
                      "founded", "leagues", "venue_name", "venue_id",
                      "venue_surface", "venue_address", "venue_city",
                      "venue_capacity", "coach_name", "coach_id")

  for (i in 1:teamListLength) {
    if (redisConnection$EXISTS(key = 'active') == 0) {
      teamID <- redisConnection$LPOP(key = 'analyseTeams')
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
      redisConnection$HMSET(key = basic, field = names(basicData), 
                            value = as.character(basicData))
      
      squadInfo <- teamData$squad
      if (length(squadInfo) > 0) {
        for (k in 1:nrow(squadInfo)) {
          playerID <- squadInfo$id[k]
          squadPlayer <- paste0(squad, ":", playerID)
          redisConnection$HMSET(key = squadPlayer, field = names(squadInfo[k, ]), 
                                value = as.character(squadInfo[k, ]))
          
          # Check if player has been added to the set for analysis later.
          # Or if it is ready to be updated after another match has been played.
          newPlayers <- redisConnection$SADD(key = paste0('comp:_playerSetInfo_:'),
                                   member = playerID)
  
          if (newPlayers == 1 || updateData) {
            redisConnection$LPUSH(key = 'analysePlayers', value = playerID)
          }
        }
      }
      redisConnection$HMSET(key = stats, field = names(teamData$statistics), 
                            value = as.character(teamData$statistics))
    }
  }
}
