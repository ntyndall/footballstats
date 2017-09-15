#' @title Predict Future Matches
#'
#' @description ...
#'  
#' @param competitionID An integer value denoting the competition ID.
#' @param seasonStarting An integer denoting the start year of the season.
#' @param returnItems A vector of character values that hold the names of
#'  fields to be returned for the commentary statistics.
#' @param SVMfit An SVM object used as the classifier for predicting future
#'  matches.
#'
#' @return ...


predictFutureMatches <- function(competitionID, seasonStarting, returnItems, SVMfit) {
  
  # Get from and to dates for future fixtures
  dateFrom <- formatDates(standardDateFormat = Sys.Date() + 1)
  dateTo <- formatDates(standardDateFormat = Sys.Date()  + 8)
  
  # Define variable names and keys
  matchFieldNames <- c('formatted_date', 'localteam_score', 'localteam_id', 'visitorteam_score', 'visitorteam_id')
  matchEndpoint <- paste0("/matches?comp_id=", competitionID, "&from_date=", dateFrom, "&to_date=", dateTo, "&")
  localVisitor <- c('localteam_id', 'visitorteam_id')
  
  # Get fixtures 
  fixtureList <- getGeneralData(endpoint = matchEndpoint)
  cat(paste0(Sys.time(), ' : About to report on results...\n'))
  
  # Loop over each fixture
  for (i in 1:row(fixtureList)) {
    singleFixture <- fixtureList[i, ]
    homeName <- singleFixture$localteam_name
    awayName <- singleFixture$visitorteam_name

    fixtureAggregate <- lapply(1:2, function(j) {
      # Decide to analyse home team and then away team
      homeOrAway <- singleFixture[[localVisitor[j]]]
      commentary <- as.character(redisConnection$KEYS(pattern = paste0('cmt*:', homeOrAway)))
      
      # Determine the statistics of a commentary
      currentStats <- commentaryStatistics(commentary = commentary,
                                           returnItems = returnItems)
      
      # Also get the match ID's
      matchIDs <- sapply(1:length(commentary), function(k) {
        strsplit(x = commentary[[k]], split = ':')[[1]][3] 
      })
      
      # Determine forms from a vector of matches
      form <- getFormFromMatchIDs(matchIDs = matchIDs)
      list(currentStats, form)
      
    })
    
    # Create the appropriate data structures for the SVM
    predictions <- sapply(1:2, function(k) {
      singleTeam <- data.frame(t(as.integer(fixtureAggregate[[k]][[1]])))
      names(singleTeam) <- returnItems
      wld <- strsplit(fixtureAggregate[[k]][[2]], '')[[1]]
      singleTeam$form <- as.integer((sum(wld == "W")*2) + sum(wld == "D"))
      predict(SVMFit, singleTeam)
    })
    
    # Predict scores now
    predictions <- as.character(predictions)
    phome <- predicitons[1]
    paway <- predicitons[2]
    
    if (phome == paway) {
      phome <- paway <- 'D'
    }
    
    txt <- as.character(paste0('[', phome, '] ', homeName, ' vs. ', awayName, ' [', paway, ']'))
    cat(paste0(Sys.time(), ' : ', txt, '\n'))
    Sys.sleep(1)
    
  }
  return(NULL)
}
