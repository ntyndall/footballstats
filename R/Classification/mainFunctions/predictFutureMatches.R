#' @title Predict Future Matches
#'
#' @description A function that queries the endpoint and redis to obtain
#'  upcoming matches then with the current optimized SVM, attempts to predict
#'  the results.
#'  
#' @param competitionID An integer value denoting the competition ID.
#' @param seasonStarting An integer denoting the start year of the season.
#' @param returnItems A vector of character values that hold the names of
#'  fields to be returned for the commentary statistics.
#' @param SVMfit An SVM object used as the classifier for predicting future
#'  matches.
#'
#' @return Nothing. Print results to screen.


predictFutureMatches <- function(competitionID, seasonStarting, returnItems, subsetItems, SVMfit) {
  
  # Get from and to dates for future fixtures
  dateFrom <- formatDates(standardDateFormat = Sys.Date() + 1)
  dateTo <- formatDates(standardDateFormat = Sys.Date()  + 8)
  
  # Define variable names and keys
  matchFieldNames <- c('formatted_date', 'localteam_score', 'localteam_id', 'visitorteam_score', 'visitorteam_id')
  matchEndpoint <- paste0("/matches?comp_id=", competitionID, "&from_date=", dateFrom, "&to_date=", dateTo, "&")
  
  # Get fixtures 
  fixtureList <- getGeneralData(endpoint = matchEndpoint)
  cat(paste0(Sys.time(), ' : About to report on results...\n'))
  
  # Generate predictions based on actual fixtures!
  numOfPredicted <- generatePredictions(fixtureList = fixtureList,
                                        testing = FALSE,
                                        returnItems = returnItems,
                                        subsetItems = subsetItems,
                                        SVMfit = SVMfit)
  print(paste0(Sys.time(), ' : Predicted a total of ', numOfPredicted, ' matches.'))
}
