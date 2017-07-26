#' @title Add Competition Standing Info
#'
#' @description A function that takes a competitionID and returns the current
#'  table information.
#'  
#' @details Competition table information is stored in the following redis hash
#'    ->   comp:season:_standing_:{comp_id}:{season}  
#'  
#' @param competitionID An integer containing the competition ID that the 
#'  teams and match information belong to.
#'  
#' @return Returns nothing, a redis hash map is set with the competition
#'  standing information.
#'


addCompetitionStandingInfo <- function(competitionID) {
  if (redisConnection$EXISTS(key = 'active') == 0) {
    standings <- getStandings(competitionID = competitionID)
    checkRequestLimit()
  } else {
    print(Sys.time(), ' : Run out of requests in addCompetitionStandingInfo()')
    standings <- NULL
  }
  
  if (!is.null(standings)) {
    for (i in 1:nrow(standings)) {
      singleTable <- standings[i, ]
      standingKey <- paste0("comp:season:_standing_:", competitionID,
                            singleTable$season)
      redisConnection$HMSET(key = standingKey, field = names(singleTable),
                            value = as.character(singleTable))
    }
  }
}
