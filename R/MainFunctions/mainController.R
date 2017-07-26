#' @title Main
#'
#' @description A function that is called from a shell script to kick off
#'  the storing of important data and also any machine learning mechanisms
#'  for prediction.
#'  
#' @details This main function is split into 3 main important components
#'  1) All libraries, global variables are loaded. Storing mechanisms
#'     that incorporate new ID keys are generated to store all useful 
#'     information for classification / algorithms.
#'  2) Statistical models are built on the stored data and redis, future
#'     fixtures are obtained and built into the models.
#'  3) Predictions are made based on the current data and models and 
#'     anything useful is sent via slack for easy analysis.
#'  Example queries carried out:
#'     ->   matches?comp_id=1204&from_date=01.01.2016&to_date=20.06.2016
#'     ->   team/9092
#'
#'  
#' @return Returns nothing.
#'


mainController <- function(redis, compID, updateData = FALSE, 
                           seasonStarting = 2015) {

  # Begin finding match information
  dateFrom <- paste0('31.07.', seasonStarting)
  dateTo <- paste0('31.07.', seasonStarting + 1)
    
  # Add competition standing
  addCompetitionStandingInfo(competition = compID)
  
  
  startingRequests <- as.integer(redis$GET(key = 'requestLimit'))
  startingTime <- redis$TTL(key = 'requestLimit')
  
  # Add match information
  matches <- addMatchInfo(competitionID = compID,
                          dateFrom = dateFrom,
                          dateTo = dateTo,
                          updateData = updateData)
  
  # Add event information
  if (nrow(matches) > 0) {
    addEventInfo(competitionID = compID,
                 matchIDs = matches$id,
                 matchEvents = matches$events)
  }
  
  # Add team information
  teamListLength <- redis$LLEN(key = 'analyseTeams')
  if (teamListLength > 0) {
    addTeamInfo(competitionID = compID,
                teamListLength = teamListLength,
                updateData = updateData)
  }
  
  # Add player information
  playerLength <- redis$LLEN(key = 'analysePlayers')
  if (playerLength > 0) {
    addPlayerInfo(competitionID = compID,
                  playerLength = playerLength)
  }
  
  # Count the number of GET requests made. 2 for competition standing and match information
  uniqueRequests <- 2
  totalRequests <- uniqueRequests + teamListLength + playerLength
  print(paste0(Sys.time(), ' : Analysed ', totalRequests, ' unique GET requests.'))
  print(paste0(Sys.time(), ' : Analysed ', length(matches$events), ' matches/events.'))
  print(paste0(Sys.time(), ' : Analysed ', teamListLength, ' teams'))
  print(paste0(Sys.time(), ' : Analysed ', playerLength, ' players.'))
  cat('\n\n')
}
