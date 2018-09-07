#' @title Classify XGBoost Setup
#'
#' @export
#'
#' @import xgboost


classify_xg_setup <- function(KEYS, singleFixture, datModel) {

  # Initialise list here
  predicted <- list(
    analysed = 0,
    nAnalysed = 0
  )

  # Build data frame from a singleFixture
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
    result.dat$res <-'U'
  }

  # If any are missing then return early
  if (all.inter.data %>% anyNA %>% `||`(all.inter.data %>% is.null)) {
    predicted$nAnalysed %<>% `+`(1)
  } else {
    predicted$analysed %<>% `+`(1)

    # Scale the results
    scaled.results <- result.dat %>%
      footballstats::scale_data(
        dataScales = footballstats::xgScales
      )

    # Determine boundaries
    scaled.results %<>%
      mltools::scaled_to_discrete(
        boundLen = KEYS$XG_BOUND
      )

    # Create a sparse matrix
    sparse.test <- scaled.results %>%
      mltools::create_sparse(
        boundLen = KEYS$XG_BOUND
      )

    # Make the prediction
    result <- predict(datModel, sparse.test)

    # Get the home team result
    predicted$home <- c('D', 'L', 'W') %>% `[`(result %>% `+`(1))
    predicted$away <- predicted$home %>% footballstats::other_score()
  }

  # Return single data frame row
  return(predicted)
}
