#' @title Optimize Features
#'
#' @description A function used to optimize data set features.
#'
#' @details This function takes a number of parameters, and goes through
#'  each unique combination in order to try and optimize the feature sets
#'  based on the total data used.
#'
#' @export


optimize_features <- function(data.set, team = 100.0, player = 0.0, days = 4, decay = 0.0, folds = 10) {

  for (i in 1:(data.set %>% nrow)) {
    # Set up variables
    matchID <- data.set$matchID[i]

    # Get player strength once developed
    if (FALSE) {
      pStrength <- matchID %>%
        footballstats::player_strength()
    }
  }

  # Carry out Neural network CV
  if (FALSE) {
    for (i in 1:folds) {
      footballstats::neural_network()
    }
  }

}
