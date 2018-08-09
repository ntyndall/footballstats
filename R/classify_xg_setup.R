#' @title Classify XGBoost Setup
#'
#' @export
#'
#' @import xgboost


classify_xg_setup <- function(KEYS, singleFixture, datModel) {



  library(xgboost)

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
  if (all.inter.data %>% anyNA %>% `!`()) {
    # do calculations here (OPTIMIZE THESE VALUES ELSEWHERE!)
    result.dat <- all.inter.data %>%
      footballstats::optimize_calculation(
        day = 3,
        gridPoints = 4,
        gridBoundary = 0.1,
        decayFactor = 1,
        til = KEYS$TIL,
        totalPer = 0.5
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
  if (all.inter.data %>% anyNA) {
    predicted$nAnalysed %<>% `+`(1)
  } else {
    predicted$analysed %<>% `+`(1)

    # Scale the results
    scaled.results <- result.dat %>%
      footballstats::scale_data(
        dataScales = footballstats::xgScales
      )

    # Determine boundaries
    scaled.results %<>% footballstats::scaled_to_discrete(
      boundLen = 4
    )

    # Create a sparse matrix
    sparse.test <- scaled.results %>%
      footballstats::create_sparse(
        boundLen = 4
      )

    # Make the prediction
    result <- predict(datModel, sparse.test)

    # Get the home team result
    resultsOrd <- c('D', 'L', 'W')
    predicted$home <- resultsOrd[result %>% `+`(1)]
    predicted$away <- predicted$home %>% footballstats::other_score()
  }

  # Return single data frame row
  return(predicted)
}
