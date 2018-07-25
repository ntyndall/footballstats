#' @title Classify Neural Setup
#'
#' @export


classify_neural_setup <- function(KEYS, singleFixture) {

  # Initialise list here
  predicted <- list(
    analysed = 0,
    nAnalysed = 0
  )

  # Get team information from fixture data frame
  matchID <- singleFixture$id %>% as.integer
  teamIDs <- c(singleFixture$localteam_id, singleFixture$visitorteam_id)

  # Need a non-null frame to start with
  matchMetrics <- data.frame(
    matchID = matchID,
    stringsAsFactors = FALSE
  )

  # Bind the commentaries together
  matchMetrics %<>% cbind(
    footballstats::project_commentaries(
      KEYS = KEYS,
      teamIDs = teamIDs,
      matchDate = singleFixture$formatted_date,
      matchID = matchID
    )
  )

  # Bind the form
  matchMetrics %<>% cbind(
    footballstats::project_form(
      KEYS = KEYS,
      teamIDs = teamIDs,
      currentID = matchID
    )
  )

  # Figure out the standings
  positions <- footballstats::feat_position(
    KEYS = KEYS,
    matchID = matchID,
    teamIDs = teamIDs,
    matchDate = singleFixture$formatted_date
  )

  matchMetrics %<>% cbind(
    data.frame(
      `position.h` = positions$position.h,
      `position.a` = positions$position.a,
      stringsAsFactors = FALSE
    )
  )

  # Use data scales pre determined by scripts
  dataScales <- footballstats::dataScales

  # If any are missing then return early
  if (matchMetrics %>% is.na %>% any) {
    predicted$nAnalysed %<>% `+`(1)
  } else {
    predicted$analysed %<>% `+`(1)
    matchMetrics$matchID <- NULL

    # Scale the data as required
    scled <- matchMetrics %>%
      scale(
        center = dataScales$sMin,
        scale = dataScales$sMax - dataScales$sMin
      ) %>%
      as.data.frame

    # Make the prediction
    result <- neuralnet::compute(
      x = footballstats::nn$neural,
      covariate = scled
    )

    # Get the home team result
    resultsOrd <- c('D', 'L', 'W')
    predicted$home <- resultsOrd[result$net.result[1, ] %>% which.max]
    predicted$away <- predicted$home %>% footballstats::other_score()
  }

  # Return single data frame row
  return(predicted)
}
