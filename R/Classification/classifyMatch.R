




classifyMatch <- function(redisConnection, competitionID, matchIDToClassify,
                          competitionClassifier) {
  keyName <- paste0('comp:matchID:_fixtures_', competitionID, ':', matchIDToClassify)
  teamIDs <- redisConnection$HMGET(key = keyName,  
                                   field = c('localteam', 'visitorteam'))
  
  
  
}