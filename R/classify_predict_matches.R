#' @title Predict Matches
#'
#' @description A function that queries the endpoint and redis to obtain
#'  upcoming matches then with the current model attempts to predict the
#'  matches that are being played between \code{DATE_FROM} and \code{DATE_TO}
#'
#' @details API endpoints;
#'   \itemize{
#'     \item{\emph{"/matches?comp_id={comp_id}&from_date={mm.dd.yyyy}&to_date={mm.dd.yyyy}&Authorization={auth_id}"}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @return Nothing. Print results to screen.
#'
#' @export


predict_matches <- function(KEYS, datModel, ..., cMethod = "xgboost") {

  # Check input first
  input <- list(...)

  # Get fixtures
  cat(paste0(Sys.time(), ' | About to report on results...\n'))
  fixtureList <- if (input %>% length %>% `>`(0)) {
    input %>% `[[`(1)
  } else { # nocov start
    paste0("/matches?comp_id=", KEYS$COMP, "&from_date=", KEYS$DATE_FROM, "&to_date=", KEYS$DATE_TO, "&") %>%
      footballstats::get_data(KEYS = KEYS)
  } # nocov end

  # Generate predictions based on actual fixtures!
  if (fixtureList %>% is.null %>% `!`()) {
    numOfPredicted <- KEYS %>%
      footballstats::generate_predictions(
        fixtureList = fixtureList,
        cMethod = cMethod,
        datModel = datModel
      )
    predictions <- numOfPredicted$analysed
    cat(paste0(Sys.time(), ' | Predicted a total of ', numOfPredicted$analysed, ' matches. \n'))
    if (KEYS$TEST) cat(paste0(Sys.time(), ' | With ', numOfPredicted$correct, ' matches guessed correctly. \n'))
  } else {
    predictions <- 0
    cat(paste0(Sys.time(), ' | No upcoming fixture in the next week! \n'))
  }

  # Return the number of predictions made
  return(predictions)
}
