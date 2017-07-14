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


addTeamInfo <- function(teamData, competition, nextTeam) {
  stats <- teamData$statistics
  myList = list(name = teamData$name,
                capacity = teamData$venue_capacity,
                coach = teamData$coach_name)
  
  as.list(stats)
  myList <- c(myList, as.list(stats))
  
  key <- paste0("teams:", competition, ":", competition, ":", nextTeam)
  rredis::redisHMSet(key = key, values = myList)
}
