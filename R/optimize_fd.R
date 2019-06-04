#' @title Optimize FD
#'
#' @details A wrapper around football data to optimize
#'  the grid etc.
#'  
#' @export


optimize_fd <- function() {
  
  
  # Set up the grids to optimise
  GRIDS <- list(
    XG_BOUND = KEYS$XG_BOUND,
    DAYS = KEYS$DAYS,
    PARAM_GPOINTS = KEYS$PARAM_GPOINTS,
    PARAM_GBOUNDARY = KEYS$PARAM_GBOUNDARY,
    PARAM_DECAY = KEYS$PARAM_DECAY,
    PARAM_TOTALPER = KEYS$PARAM_TOTALPER
  )
  
  # Create the metric data set
  metrics <- create_football_data(
    GRIDS = GRIDS
  )
  
  # Use metrics to build XGB model
}