#' @title Classify Method Selection
#'
#' @description A function to decide which model to use
#'  during the classification
#'
#' @export


classify_method_selection <- function(KEYS, method = "", singleFixture, datModel) {

  # Allowed types
  allowedMethods <- c("xgboost", "neuralnetwork")

  # Multiple selections here
  predicted <- if (method == "xgboost") {
    predicted <- KEYS %>%
      footballstats::classify_xg_setup(
        singleFixture = singleFixture,
        datModel = datModel
      )
  } else if (method == "neuralnetwork") {
    KEYS %>%
      footballstats::classify_neural_setup(
        singleFixture = singleFixture
      )
  } else {
    KEYS %>%
      footballstats::classify_xg_setup(
        singleFixture = singleFixture
      )
  }

  return(predicted)
}
