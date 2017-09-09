



buildGeneralClassifier <- function(redisConnection, competitionID, matchData, seasonStarting) {
  # Query Redis and return everything from the competition. 
  matchData <- recreateMatchData(redisConnection = redisConnection, 
                                 competitionID = competitionID, 
                                 seasonStarting = seasonStarting)
  
  # Only look back at the previous `x` matches.
  if (nrow(matchData) == 0) {
    return(paste0(Sys.time(), ' : No match data found for the providing input parameters.'))
  } else if (nrow(matchData) > 50) {
    matchData <- matchData[1:50, ]
  }

  # items to analyse:
  returnItems <- c('shots_total', 'shots_ongoal', 'fouls', 'corners', 'possesiontime', 'yellowcards', 'saves')
  # _REDIS_ : [ctm_commentary]:{comp_id}:{match_id}:{team_id} 
  commentaryKeys <- as.character(redisConnection$KEYS(pattern = 'cmt_commentary*'))
  
  totalData <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:length(commentaryKeys)) {
    singleCommentary <- commentaryKeys[i]
    elementsSplit <- strsplit(singleCommentary, ':')[[1]]
    matchID <- elementsSplit[3]
    teamID <- elementsSplit[4]
    results <- redisConnection$HMGET(key = singleCommentary,
                                     field = returnItems)
    names(results) <- returnItems
    results$possesiontime <- gsub(pattern = "%", replacement = "", results$possesiontime)
    resultsAsNums <- as.double(results)
    
    matchInfo <- redisConnection$HGETALL(key = paste0('csm:', competitionID, ':', seasonStarting, '/2018:', matchID))
    singleMatchInfo <- matchInfo[c(FALSE, TRUE)]
    names(singleMatchInfo) <- matchInfo[c(TRUE, FALSE)]
    
    homeAway <- names(which(singleMatchInfo == teamID))
    if (homeAway == "localteam_id") {
      
    } else {
      
    }
    
    if (i == 1) {
      
    } else {
      
    }
  }
}

