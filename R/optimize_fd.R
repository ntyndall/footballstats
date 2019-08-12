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

  all.metrics <- list(
    train = data.frame(stringsAsFactors = FALSE),
    meta = data.frame(stringsAsFactors = FALSE)
  )
  # Create the metric data set
  for (type in c('all', '2018-2019')) {
    metrics <- create_football_data(
      GRIDS = GRIDS,
      type = type
    )
    all.metrics$train %<>% rbind(metrics$train)
    all.metrics$meta %<>% rbind(metrics$meta)
  }

  # Get all the unique leagues
  uniqueLeagues <- all.metrics$meta$zzz.division %>%
    unique






  ####
  ####  Do it season by season
  ####
  resultsToPlot2 <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:(uniqueLeagues %>% length)) {
    print(uniqueLeagues[i])
    lgc <- all.metrics$meta$zzz.division %>% `==`(uniqueLeagues[i])

    sub.train <- all.metrics$train %>% subset(lgc)
    sub.meta <- all.metrics$meta %>% subset(lgc)

    uniqueSeasons <- sub.meta$zzz.season %>% unique
    if (length(uniqueSeasons) < 3) next

    for (j in 1:(uniqueSeasons %>% length)) {
      print(paste0("TESTING SEASON ", uniqueSeasons[j]))
      lgc2 <- sub.meta$zzz.season == uniqueSeasons[j]

      train.data <- sub.train %>% subset(!lgc2)
      test.data <- sub.train %>% subset(lgc2)
      test.meta <- sub.meta %>% subset(lgc2)

      outcomes <- build_and_test_fd(train.data = train.data, test.data = test.data, test.meta = test.meta)


      resultsToPlot2 %<>% rbind(test.meta %>% cbind(outcomes))
    }
  }

  ###
  ### Build a model using all data - the league and season of interest!
  ###
  everyOption <- all.metrics$meta %>%
    dplyr::group_by(zzz.season, zzz.division) %>%
    dplyr::summarise()

  # I am up to 114
  resultsToPlot3 <- data.frame(stringsAsFactors = FALSE)
  for (i in 114:(everyOption %>% nrow)) {
    subsetBy <- everyOption[i, ] %>%
      as.character

    print(i)
    lgc <- all.metrics$meta$zzz.season %>%
      `==`(subsetBy[1]) %>%
      `&`(all.metrics$meta$zzz.division %>% `==`(subsetBy[2]))

    train.data <- all.metrics$train %>% subset(!lgc)
    test.data <- all.metrics$train %>% subset(lgc)
    test.meta <- all.metrics$meta %>% subset(lgc)

    outcomes <- build_and_test_fd(
      train.data = train.data,
      test.data = test.data,
      test.meta = test.meta
    )

    resultsToPlot3 %<>% rbind(test.meta %>% cbind(outcomes))
  }

  # Get logical vector
  resultsToPlot <- resultsToPlot3
  save(resultsToPlot, file = "~/Documents/fstats.dashboard/totalResults2.rda")

  # Use metrics to build XGB model

}
