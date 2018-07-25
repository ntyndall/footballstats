#' @title Classify XGBoost Setup
#'
#' @export


classify_xg_setup <- function(KEYS, singleFixture) {

  # Initialise list here
  predicted <- list(
    analysed = 0,
    nAnalysed = 0
  )

  # Get team information from fixture data frame
  matchID <- singleFixture$id %>% as.integer
  teamIDs <- c(singleFixture$localteam_id, singleFixture$visitorteam_id)

  # Build data set here...
  # ...


  # Use data scales pre determined by scripts
  dataScales <- footballstats::dataScales

  # If any are missing then return early
  if (matchMetrics %>% is.na %>% any) {
    predicted$nAnalysed %<>% `+`(1)
  } else {
    predicted$analysed %<>% `+`(1)
    matchMetrics$matchID <- NULL
    matchMetrics$res <- 'U'

    # Scale the data as required
    scaled.data <- matchMetrics %>%
      scale(
        center = dataScales$sMin,
        scale = dataScales$sMax - dataScales$sMin
      ) %>%
      as.data.frame

    # Create a sparse matrix
    sparse.test <- scaled.data %>%
      footballstats::create_sparse(
        boundLen = 4
      )

    # Make the prediction
    result <- predict(footballstats::xgb, sparse.test)

    # Get the home team result
    resultsOrd <- c('D', 'L', 'W')
    predicted$home <- resultsOrd[result]
    predicted$away <- predicted$home %>% footballstats::other_score()
  }

  # Return single data frame row
  return(predicted)
}
