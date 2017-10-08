#' @title Get Home And Away Stats
#'
#' @description A function to set up a match performance of two teams which returns
#'  their current statistics and their form.
#'  
#' @param singleFixture A single row data frame containing match localteam vs. 
#'  visitorteam information which CAN include actual results if testing == TRUE.
#' @param localVisitor A character vector which contains one of 
#'  c('localteam_id', 'visitorteam_id').
#' @param testing A boolean value which decides whether to read the actual result
#'  of the match and compare with the classifier.
#'
#' @return A list of the current stats in key 1 and form in key 2.
#'


getHomeAndAwayStats <- function(singleFixture, seasonStarting, localVisitor, returnItems, 
                                matchFieldNames, testing) {
  fixtureAggregate <- lapply(1:2, function(j) {
    # Decide to analyse home team and then away team
    homeOrAway <- singleFixture[[localVisitor[j]]]
    commentary <- as.character(redisConnection$KEYS(pattern = paste0('cmt*:', homeOrAway)))
    
    # For testing only: Don't include the very last commentary!
    if (testing) {
      commentary <- commentary[1:(length(commentary) - 1)]
    }
  
    # Determine the statistics of a commentary
    currentStats <- commentaryStatistics(commentary = commentary,
                                         returnItems = returnItems)
    
    # Also get the match ID's
    matchIDs <- sapply(1:length(commentary), function(k) {
      strsplit(x = commentary[[k]], split = ':')[[1]][3] 
    })
    
    # Determine forms from a vector of matches
    form <- getFormFromMatchIDs(matchIDs = matchIDs,
                                seasonStarting = seasonStarting,
                                matchFieldNames = matchFieldNames)
    list(currentStats, form)
  })
  return(fixtureAggregate)
}
