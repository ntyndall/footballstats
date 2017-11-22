#'
#' @export


evaluatedPredictionsToSlack <- function(competitionID, competitionName, seasonStarting) {
  readyToEvaluate <- as.character(rredis::redisKeys(
    pattern = 'c:', competitionID, ':ready:*'))
  strsplit(x = readyToEvaluate, split = ':')
  for (i in 1:length(readyToEvaluate)) {
    matchID <- strsplit(x = readyToEvaluate, split = ':')[[4]]

    predictResults <- rredis::redisHMGet(
      key = paste0('c:', competitionID, ':pred:', matchID),
      fields = c('home', 'away'))

    matchKey <- paste0("csm:", competitionID, ":", seasonStarting, ":", matchID)

    actualResults <- rredis::redisHMGet(
      key = matchKey,
      fields = c('localteam', '....'))

    # Convert actual score to 'L' / 'W' / 'D'
    # ...
  }

}
