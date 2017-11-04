




evaluatedPredictionsToSlack <- function(competitionID, competitionName, seasonStarting) {
  readyToEvaluate <- as.character(redisConnection$KEYS(pattern = 'c:', competitionID, ':ready:*'))
  strsplit(x = readyToEvaluate, split = ':')
  for (i in 1:length(readyToEvaluate)) {
    matchID <- strsplit(x = readyToEvaluate, split = ':')[[4]]
    predictResults <- redisConnection$HMGET(key = paste0('c:', competitionID, ':pred:', matchID), 
                                            field = c('home', 'away'))
    matchKey <- paste0("csm:", competitionID, ":", seasonStarting, ":", matchID)
    actualResults <- redisConnection$HMGET(key = matchKey, 
                                           field = c('localteam', '....'))
    
    # Convert actual score to 'L' / 'W' / 'D'
    # ...
  }
  
}