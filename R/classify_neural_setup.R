#' @title Classify Neural Setup
#'
#' @export


classify_neural_setup <- function(KEYS, singleFixture, datModel) {

  # Initialise list here
  predicted <- list(
    analysed = 0,
    nAnalysed = 0
  )

  # Calculate actual data set
  all.inter.data <- KEYS %>%
    footballstats::build_raw_data(
      singleFixture = singleFixture
    )

  # NA'S EXIST
  if (all.inter.data %>% anyNA %>% `!`() %>% `&&`(all.inter.data %>% is.null %>% `!`())) {
    # do calculations here (OPTIMIZE THESE VALUES ELSEWHERE!)
    result.dat <- all.inter.data %>%
      footballstats::optimize_calculation(
        day = KEYS$DAYS,
        gridPoints = KEYS$PARAM_GPOINTS,
        gridBoundary = KEYS$PARAM_GBOUNDARY,
        decayFactor = KEYS$PARAM_DECAY,
        til = KEYS$TIL,
        totalPer = KEYS$PARAM_TOTALPER
      )

    # Now I need positions for the two current teams!!
    positions <- footballstats::feat_position(
      KEYS = KEYS,
      matchID = singleFixture$id,
      teamIDs = c(singleFixture$localteam_id, singleFixture$visitorteam_id),
      matchDate = singleFixture$formatted_date
    )

    # Append them on
    result.dat$`position.h` <- positions$`position.h` %>% `/`(KEYS$TIL)
    result.dat$`position.a` <- positions$`position.a` %>% `/`(KEYS$TIL)
  }

  # If any are missing then return early
  if (all.inter.data %>% anyNA %>% `||`(all.inter.data %>% is.null)) {
    predicted$nAnalysed %<>% `+`(1)
  } else {
    predicted$analysed %<>% `+`(1)

    # Scale results
    scaled.results <- result.dat %>%
      footballstats::scale_data(
        dataScales = footballstats::nnScales
      )

    # Calculate results
    result <- neuralnet::compute(
      x = datModel,
      covariate = scaled.results
    )

    # Get the home team result
    resultsOrd <- c('D', 'L', 'W')
    predicted$home <- resultsOrd[result$net.result[1, ] %>% which.max]
    predicted$away <- predicted$home %>% footballstats::other_score()
  }

  # Return single data frame row
  return(predicted)
}
