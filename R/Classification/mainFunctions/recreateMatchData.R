#' @title Recreate Match Data
#' 
#' @description A function that provided with a competitionID and season start year,
#'  can auto generate and order by date all the basic match information
#'  
#' @details A search for all matches in a particular subset is made in Redis, a data frame
#'  is then constructed to rebuild the original API query and ordered by date.
#'  
#' @param competitionID An integer containing the competitionID that the 
#'  teams and match information belong to.
#' @param seasonStarting An integer defining the lower year for details on a season.
#' @param redisData An environment that defines the redis configuration where data is
#'  to be searched for.  
#'  
#' @return matchData. A data frame containing all the matches in a particular season.
#' 


recreateMatchData <- function(redisConnection, competitionID, seasonStarting, matchLimit) {
  allMatches <- redisConnection$KEYS(pattern = paste0('csm:', competitionID, ':', seasonStarting, '*'))
  matchData <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:length(allMatches)) {
    singleMatch <- redisConnection$HGETALL(key = allMatches[i])
    singleMatch <- as.character(singleMatch)
  
    matchID <- data.frame(t(singleMatch[c(FALSE, TRUE)]),
                          stringsAsFactors = FALSE)
    matchIDName <- singleMatch[c(TRUE, FALSE)]
    names(matchID) <- matchIDName
    if (i == 0) {
      matchData <- matchID
    } else {
      matchData <- rbind(matchData, matchID)
    }
  }
  # Re-order the dataframe by date.
  matchData$formatted_date <- as.Date(matchData$formatted_date, '%d.%m.%Y')
  matchData <- matchData[order(matchData$formatted_date), ]
  
  # Only look back at the previous `x` matches.
  if (nrow(matchData) == 0) {
    print(paste0(Sys.time(), ' : No match data found for the providing input parameters.'))
    matchData <- data.frame(stringsAsFactors = FALSE)
  } else if (nrow(matchData) > matchLimit) {
    matchData <- matchData[1:matchLimit, ]
  }
  return(matchData)
}
