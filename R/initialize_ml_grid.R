#' @title Initialize ML Grid
#'
#' @description A function for returning the grid to be used
#'  in optimizing the ML models.
#'
#' @export


initialize_ml_grid <- function() { # nocov start
  return(
    list(
      DAYS = c(3, 4, 5),
      GRID_PTS = c(2, 4, 6, 8, 10),
      GRID_BOUND = c(0.1, 0.2, 0.3),
      DECAY = c(1, 2, 5000),
      TOTAL_PERC = seq(from = 0.0, to = 1.0, by = 1),
      NN_REP = 1,
      NN_THRESH = 0.18,
      XG_ROUNDS = 40000,
      XG_DEPTH = 10,
      XG_ETA = 0.2,
      XG_GAMMA = 2
    )
  )
} # nocov end
