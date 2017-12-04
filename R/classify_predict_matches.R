#' @title classify_predict_matches
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
#'
#' @export


predict_matches <- function(competitionID, competitionName, seasonStarting, returnItems,
                            matchFieldNames, subsetItems, SVMfit, binList, printToSlack, KEYS) {

  # Get from and to dates for future fixtures
  dateFrom <- footballstats::format_dates(
    standardDateFormat = Sys.Date() + 1)
  dateTo <- footballstats::format_dates(
    standardDateFormat = Sys.Date()  + 8)

  # Define variable names and keys
  matchFieldNames <- c('formatted_date', 'localteam_score', 'localteam_id', 'visitorteam_score', 'visitorteam_id')
  matchEndpoint <- paste0("/matches?comp_id=", competitionID, "&from_date=", dateFrom, "&to_date=", dateTo, "&")

  # Get fixtures
  if (KEYS %>% is.null) {
    fixtureList <- footballstats::matchData[60:70, ]
  } else {
    fixtureList <- footballstats::get_data(
      endpoint = matchEndpoint,
      KEYS = KEYS)
    cat(paste0(Sys.time(), ' : About to report on results...\n'))
  }

  # Generate predictions based on actual fixtures!
  if (!is.null(fixtureList)) {
    numOfPredicted <- footballstats::generate_predictions(
      competitionID = competitionID,
      fixtureList = fixtureList,
      seasonStarting = seasonStarting,
      testing = FALSE,
      returnItems = returnItems,
      subsetItems = subsetItems,
      SVMfit = SVMfit,
      matchFieldNames = matchFieldNames,
      competitionName = competitionName,
      binList = binList,
      printToSlack = printToSlack,
      KEYS = KEYS,
      real = TRUE)
    print(paste0(Sys.time(), ' : Predicted a total of ', numOfPredicted, ' matches.'))
  } else {
    print(paste0(Sys.time(), ' : No upcoming fixture in the next week!'))
  }
}
