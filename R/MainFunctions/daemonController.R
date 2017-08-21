#' @title Daemon Controller
#'
#' @description ...
#'  
#' @details ...
#' 
#' @param redisConnection An environment which contains all redis call
#'  information.
#' @param competitionID A unique character string of length four denoting
#'  the competition ID.
#' @param currentDate A POSIXCT value that defines the date currently under
#'  analysis.
#' @param updateData A boolean value that defines whether to update data
#'  e.g. for a players statistics. Usually set to TRUE.
#'
#' @return Returns nothing.
#'


daemonController <- function(redisConnection, competitionID, currentDate, 
                             updateData = TRUE) {
  
  # Get todays date information for constructing GET requests later
  dateTo <- dateFrom <- formatDates(standardDateFormat = currentDate)
  analysingToday <- as.integer(currentDate) == as.integer(Sys.Date())
  
  # -------------------- #
  # Begin Analysing Data #
  # -------------------- #

  # Get request limit information
  startingRequests <- as.integer(redisConnection$GET(key = 'requestLimit'))
  startingTime <- redisConnection$TTL(key = 'requestLimit')
  
  # Add match information
  matches <- addMatchInfo(competitionID = competitionID,
                          dateFrom = dateFrom,
                          dateTo = dateTo,
                          updateData = updateData,
                          analysingToday = analysingToday)
  
  # Store last active date if query was successful
  if (nrow(matches) > 0) {
    redisConnection$SET(key = paste0(competitionID, ':lastActiveDate'),
                        value = as.integer(currentDate))
  }
  
  # Add event information
  if (nrow(matches) > 0) {
    addEventInfo(competitionID = competitionID,
                 matchIDs = matches$id,
                 matchEvents = matches$events)
  }
  
  # Add team information
  teamListLength <- redisConnection$LLEN(key = 'analyseTeams')
  if (teamListLength > 0) {
    addTeamInfo(competitionID = competitionID,
                teamListLength = teamListLength,
                updateData = updateData)
  }
  
  # Add player information
  playerLength <- redisConnection$LLEN(key = 'analysePlayers')
  if (playerLength > 0) {
    addPlayerInfo(competitionID = competitionID,
                  playerLength = playerLength)
  }

  # ------------------ #
  # End Analysing Data #
  # ------------------ #
}
