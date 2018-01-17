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


predict_matches <- function(competitionID, competitionName, dataScales, classifyModel, KEYS) {

  # Get from and to dates for future fixtures
  dateFrom <- (Sys.Date() + 1) %>% footballstats::format_dates()
  dateTo <- (Sys.Date()  + 8) %>% footballstats::format_dates()

  # Get fixtures
  cat(paste0(Sys.time(), ' | About to report on results...\n'))
  fixtureList <- if (KEYS$TEST) {
    footballstats::matchData[60:70, ]
  } else { # nocov start
    paste0("/matches?comp_id=", competitionID, "&from_date=", dateFrom, "&to_date=", dateTo, "&") %>%
      footballstats::get_data(
        KEYS = KEYS)
  } # nocov end

  # Generate predictions based on actual fixtures!
  if (fixtureList %>% is.null %>% `!`()) {
    numOfPredicted <- footballstats::generate_predictions(
      fixtureList = fixtureList,
      classifyModel = classifyModel,
      dataScales = dataScales,
      competitionName = competitionName,
      KEYS = KEYS)
    cat(paste0(Sys.time(), ' | Predicted a total of ', numOfPredicted, ' matches. \n'))
  } else {
    cat(paste0(Sys.time(), ' | No upcoming fixture in the next week! \n'))
  }
}
