
# Build a classifier that takes features and use the factors 1 (WIN), 0 (DRAW), and -1 (LOSE)
# take into effect..


classifyFixtures <- function(redisConnection, competitionID, dateFrom, dateTo) {
  
  # 1) Query to get fixture data
  fixtures <- getMatches(competitionID = competitionID, 
                         dateFrom = dateFrom, 
                         dateTo = dateTo)
  
  # 2) Has the data been analysed yet?
  if (nrow(fixtures) > 0) {
    toAnalyse <- matchesToAnalyse(redisConnection = redisConnection,
                                  competitionID = competitionID,
                                  fixtures = fixtures)
  }
  
  # 3) Build models based on previous data (Group together per competition)
  if (length(toAnalyse) > 0) {
    
    competitionClassifier <- buildGeneralClassifier(redisConnection = redisConnection,
                                                    competitionID = competitionID)
    
    for (i in 1:length(toAnalyse)) {
      matchIDToClassify <- toAnalyse[i]
      keyName <- paste0('comp:matchID:_fixtures_', competitionID, ':', matchIDToClassify)
      teamIDs <- redisConnection$HMGET(key = keyName,  
                                       field = c('localteam', 'visitorteam'))
      result <- classifyMatch(redisConnection = redisConnection,
                              competitionID = competitionID,
                              matchIDToClassify = toAnalyse[i],
                              competitionClassifier = competitionClassifier)
    }
  }
  
  # 4) Send data to Slack if anything has been determined
  
}