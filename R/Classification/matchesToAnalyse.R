



matchesToAnalyse <- function(redisConnection, competitionID, fixtures) {
  toAnalyse <- c()
  for (i in 1:nrow(fixtures)) {
    keyName <- paste0('comp:matchID:_fixtures_', competitionID, ':', fixtures$id[i])
    toClassify <- redisConnection$EXISTS(key = keyName)
    
    if (toClassify == 0) {
      # Add, but expire after 4 days time incase it is picked up by a later query
      redisConnection$HMSET(key = keyName, 
                            field = c('localteam', 'visitorteam', 'localname', 'visitorname'),
                            value = c(fixtures$localteam_id[i], fixtures$visitorteam_id[i],
                                      fixtures$localteam_name[i], fixtures$visitorteam_name[i]))
      redisConnection$EXPIRE(key = keyName, seconds = 60 * 60 * 24 * 4)
      toAnalyse <- c(toAnalyse, fixtures$id[i])
    }
  }
  return(toAnalyse)
}