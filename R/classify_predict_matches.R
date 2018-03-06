#' @title classify_predict_matches
#'
#' @description A function that queries the endpoint and redis to obtain
#'  upcoming matches then with the current optimized SVM, attempts to predict
#'  the results.
#'
#' @param competitionID An integer value denoting the competition ID.

#' @return Nothing. Print results to screen.
#'
#' @export


predict_matches <- function(KEYS) {

  # Get fixtures
  cat(paste0(Sys.time(), ' | About to report on results...\n'))
  fixtureList <- if (KEYS$TEST) {
    footballstats::matchData[60:70, ]
  } else { # nocov start
    paste0("/matches?comp_id=", KEYS$COMP, "&from_date=", KEYS$DATE_FROM, "&to_date=", KEYS$DATE_TO, "&") %>%
      footballstats::get_data(KEYS = KEYS)
  } # nocov end

  # Generate predictions based on actual fixtures!
  if (fixtureList %>% is.null %>% `!`()) {
    numOfPredicted <- KEYS %>%
      footballstats::generate_predictions(
        fixtureList = fixtureList
      )
    cat(paste0(Sys.time(), ' | Predicted a total of ', numOfPredicted, ' matches. \n'))
  } else {
    cat(paste0(Sys.time(), ' | No upcoming fixture in the next week! \n'))
  }
}
