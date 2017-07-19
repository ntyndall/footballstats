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


mainController <- function(production = TRUE, seasonStarting = 2015) {
  initializeAllFunctions()
  if (production) {
    dataStore = 10
  } else {
    dataStore = 1
  }
  redis <<- redux::hiredis(db = dataStore)
  
  # Get competition information
  nextDate <- redis$GET(key = "competition:waitForNextQuery")
  if (is.null(nextDate) || Sys.Date() > nextDate) {
    addCompetitionInfo(daysUntilNextQuery = 30)
  }

  # Load all current competitions for analysis
  competitionsToAnalyse  <- redis$SMEMBERS(key = 'competition:set')

  # Begin finding match information
  dateFrom <- paste0('31.07.', seasonStarting)
  dateTo <- paste0('31.07.', seasonStarting + 1)
  
  for (i in 1:length(competitionsToAnalyse)) {
    compID <- competitionsToAnalyse[[i]]

    # Add match information
    # Matches will control everything! So if a new match is found
    # then 1) add the events. 2) Check a new team is played. (ALWAYS UPDATE SQUAD LIST)
    # 3) ALWAYS update player information....
    matches <- addMatchInfo(competitionID = compID,
                            dateFrom = dateFrom,
                            dateTo = dateTo)
    
    # Add event information
    if (nrow(matches) > 0) {
      print(paste0(Sys.time(), ' : Analysing ', length(matches$events), ' events.'))
      addEventInfo(competitionID = compID,
                   matchIDs = matches$id,
                   matchEvents = matches$events)
    }
    
    # Add team information
    teamListLength <- redis$LLEN(key = 'analyseTeams')
    if (teamListLength > 0) {
      print(paste0(Sys.time(), ' : Analysing ', teamListLength, ' new team info.'))
      addTeamInfo(competitionID = compID,
                  teamListLength = teamListLength)
    }
    
    # Add player information
    playerLength <- redis$LLEN(key = 'analysePlayers')
    if (playerLength > 0) {
      print(paste0(Sys.time(), ' : Analysing ', playerLength, ' new players.'))
      addPlayerInfo(competitionID = compID,
                    playerLength = playerLength)
    }
  }
}
