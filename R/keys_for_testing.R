#' @title Keys For Testing
#'
#' @description A function that sets up the \code{KEYS}
#'  variable used throughout the package, with default parameters
#'  that avoid API calls and printing to screen etc.
#'
#' @return A list of important key values, accessible through KEYS$...
#'
#' @export


keys_for_testing <- function(dbnum = 3) {
  return(
    list(
      COMP = 1204,
      COMP_NAME = "test-competition",
      SEASON = 2017,
      DATE_FROM = NULL,
      DATE_TO = NULL,
      SLACK_PRNT = FALSE,
      TEST = TRUE,
      LOG_PRED = FALSE,
      LOGGING = FALSE,
      DAYS = 3,
      STAND = 0.5,
      TIL = 20,
      RED = redux::hiredis(db = dbnum),
      PIPE = redux::redis
    )
  )
}
