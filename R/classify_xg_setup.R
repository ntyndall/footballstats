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

    all.inter.data %<>%
      dplyr::rename(
        zzz.matchID = matchID,
        zzz.date = date,
        home.team = localName,
        home.id = localID,
        away.team = awayName,
        away.id = awayID,
        home.score = localScore,
        away.score = awayScore,
        `away.ontarget` = `shots_ongoal.a`,
        `home.ontarget` = `shots_ongoal.h`,
        `home.shots` = `shots_total.h`,
        `away.shots` = `shots_total.a`,
        zzz.result = result,
        zzz.til = til
      )

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
      matchID = singleFixture$zzz.matchID,
      teamIDs = c(singleFixture$home.id, singleFixture$away.id),
      matchDate = singleFixture$zzz.date
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

    # Not sure about a re-scaling..
    # Scale the results
    #scaled.results <- result.dat %>%
    #  footballstats::scale_data(
    #    dataScales = footballstats::xgScales
    #  )

    result.dat %<>%
      mltools::scaled_to_discrete(
        boundLen = KEYS$XG_BOUND
      )

    # Determine boundaries
    #result.dat %>%
    #  mltools::scaled_to_discrete(
    #    boundLen = KEYS$XG_BOUND
    #  )

    # Create a sparse matrix
    sparse.test <- result.dat %>%
      mltools::create_sparse(
        boundLen = KEYS$XG_BOUND
      )

    # Make the prediction
    result <- predict(datModel, sparse.test)

    # Get the home team result
    # predicted$home <- c('D', 'L', 'W') %>% `[`(result %>% `+`(1))
    # predicted$away <- predicted$home %>% footballstats::other_score()
    predicted$home <- if (result == 1)  'W' else 'NW'
    predicted$away <- if (result == 0)  'NW' else 'W'
  }

  # Return single data frame row
  return(predicted)
}
