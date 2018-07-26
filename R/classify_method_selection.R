#' @title Classify Method Selection
#'
#' @description A function to decide which model to use
#'  during the classification
#'
#' @export


classify_method_selection <- function(KEYS, method = "", singleFixture) {

  # Allowed types
  allowedMethods <- c("xgboost", "neuralnetwork")

  # Load the XGBoost model from disk
  xgModel <- xgboost::xgb.load("xgModel")
  xgScales <- load(file = "xgScales.rda")

  # Multiple selections here
  predicted <- if (method == "xgboost") {
    predicted <- KEYS %>%
      footballstats::classify_xg_setup(
        xgModel = xgModel,
        xgScales = xgScales,
        singleFixture = singleFixture
      )
  } else if (method == "neuralnetwork") {
    KEYS %>%
      footballstats::classify_neural_setup(
        singleFixture = singleFixture
      )
  } else {
    KEYS %>%
      footballstats::classify_xg_setup(
        xgModel = xgModel,
        xgScales = xgScales,
        singleFixture = singleFixture
      )
  }

  return(predicted)
}
