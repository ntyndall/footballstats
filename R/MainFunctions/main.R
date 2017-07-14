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


main <- function() {
  initializeAllFunctions()
  rredis::redisSelect(0)
  
  # Get competition information
  nextDate <- rredis::redisGet(key = "competition:waitForNextQuery")
  if (is.null(nextDate) || Sys.Date() > nextDate) {
    getAllCompetitions(daysUntilNextQuery = 30)
  }
  
  # Get match information ## NEED A BOUNDARY CONDITION HERE!!!
  lastMatchTime <- rredis::redisGet(key = "match:lastInterval")
  if (is.null(lastMatchTime)) {
    lastMatchTime <- Sys.Date() - (365 * 2)
  } else {
    as.Date(lastMatchTime, origin = "1970-01-01")
  }
  timeFrom <- format(lastMatchTime, "%d.%m.%Y") 
  timeTo <- format(lastMatchTime + 7, "%d.%m.%Y")
  rredis::redisSet(key = "match:lastInterval", 
                   value = as.integer(lastMatchTime))
  # Get match information
  getMatchInformation()
  
  # Add teams to Redis
  
  
  
  newTeams <- data.frame()
  for (i in 1:nrow(myData)) {
    rredis::redisSelect(0)
    dataRow <- myData[i, ]
    nextMatchID <- getNextID(IDLookup = "match")
    nextEventID <- getNextID(IDLookup = "event")
    
    matchResults <- addMatchInfo(dataRow, competition, nextMatchID, nextEventID)
    mapAPIToMyID(dataRow$id, nextMatchID, "match")
    
    teamInSet <- rredis::redisSIsMember(set = paste0("teams:", competition), 
                           element = dataRow$localteam_id)
  
    # Add unseen team to set
    if (!teamInSet) {
      rredis::redisSAdd(set = paste0("teams:", competition),
                        element = charToRaw(dataRow$localteam_id))
      newTeams <- rbind(newTeams, data.frame(team_id = dataRow$localteam_id))
    }
  
    # Add events
    events <- dataRow$events[[1]]
    if (nrow(events) > 0) {
      for (x in 1:nrow(events)) {
        nextSingleEventID <- getNextID(IDLookup = "singleEvent")
        addEventInfo(events[x, ], nextSingleEventID, nextEvent)
        mapAPIToMyID(events$id[x], nextSingleEventID, "singleEvent")
      }
    }
  }

  # Add home team to Redis
  if (nrow(newTeams) > 0) {
    for (i in 1:nrow(newTeams)) {
      singleTeam <- as.character(newTeams$team_id[1])
      teamData <- httr::GET(paste0(rootURL, "team/", singleTeam, authKey))
      teamData <- jsonlite::fromJSON(rawToChar(teamData$content))
      nextTeamID <- getNextID(IDLookup = "team")
      addTeamInfo(teamData, nextTeamID)
    }
  }
  
  # Add player information based on home team to Redis
  singlePlayer <- httr::GET(paste0(HOST, "/player/237?", API_KEY))
  if (singlePlayer$status_code == 200) {
   playerInformation <- jsonlite::fromJSON(rawToChar(singlePlayer$content))
   nextPlayerID <- getNextID(IDLookup = "player")
   mapAPIToMyID(playerInformation$id, nextPlayerID, "player")
   
  }
}  
