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

  # SPlit by competition and subset
  allComps <- data.set$comp_id %>% unique
  subs.data <- data.set %>% subset(allComps[1] == data.set$comp_id)

  # Now look at subs.data

  for (i in 1:(data.set %>% nrow)) {
    # Set up variables
    matchID <- data.set$matchID[i]

    # Get player strength once developed
    if (FALSE) {
      pStrength <- matchID %>%
        footballstats::player_strength()
    }

    # Get team position once developed
    if (FALSE) {
      pStrength <- matchID %>%
        footballstats::position()
    }

  }

  # Carry out Neural network CV
  if (FALSE) {
    for (i in 1:folds) {
      footballstats::neural_network()
    }
  }

}
