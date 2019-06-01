#' @title Optimize Variables
#'
#' @export


optimize_variables <- function(total.metrics, GRIDS, optimizeModels = TRUE,
                               overwrite = FALSE, types = c("xgboost", "neuralnetwork"),
                               saveModels = c(), colNames = list(localID = "localID", awayID = "awayID")) {

  # Create directory
  if (saveModels %>% length %>% `>`(0)) {
    modelDir <- getwd() %>% paste0("/mymodels/")
    if (modelDir %>% dir.exists %>% `!`()) modelDir %>% dir.create
  }

  # Only save models when requested
  if (optimizeModels) {
    if (saveModels %>% length %>% `>`(0)) { # nocov start
      cat(" ## If you want to save any models, then set optimizeModels to FALSE! \n\n")
      saveModels <- c()
    } # nocov end
  } else {
    saveModels %<>% intersect(types)
  }

  # Must supply a valid type
  if (types %>% length %>% `>`(0) %>% `!`()) stop("Must supply some _types_")

  # Redfine list from GRIDS
  DAYS <- GRIDS$DAYS
  GRID_PTS <- GRIDS$GRID_PTS
  GRID_BOUND <- GRIDS$GRID_BOUND
  DECAY <- GRIDS$DECAY
  TOTAL_PERC <- GRIDS$TOTAL_PERC

  # Only write to files if need be
  if (optimizeModels) { # nocov start
    # Make sure the base directory exists
    resultsDir <- getwd() %>% paste0("/results_optimization/")
    if (resultsDir %>% dir.exists %>% `!`()) resultsDir %>% dir.create

    # Read in the existing data frame of results here
    resultsFile <- resultsDir %>% paste0("results.csv")
    if (resultsFile %>% file.exists) {

      # Read the CSV
      existing.topscore <- read.csv2(
        file = resultsFile,
        header = TRUE,
        sep = ',',
        stringsAsFactors = FALSE
      )[ , 1:6]

      # Get matches with existing data frame
      totMatches <- existing.topscore %>%
        footballstats::get_grid_matches(
          fullGrid = expand.grid(DAYS, GRID_PTS, GRID_BOUND, DECAY, TOTAL_PERC, types)
        )
    } else {
      totMatches <- 0
    }
  } else {
    totMatches <- 0
  } # nocov end

  # Define neural network input list
  NN <- list(
    REP = GRIDS$NN_REP,
    THRESH = GRIDS$NN_THRESH %>% min(0.3)
  )

  # Define XGBoost input list
  XGB <- list(
    ROUNDS = GRIDS$XG_ROUNDS,
    DEPTH = GRIDS$XG_DEPTH,
    ETA = GRIDS$XG_ETA,
    GAMMA = GRIDS$XG_GAMMA,
    BOUNDARY = GRIDS$XG_BOUNDARY
  )

  # Initialise values for generating and tracking results
  bestResult <- icount <- 0
  totalOps <- (DAYS %>% length) *
    (GRID_PTS %>% length) *
    (GRID_BOUND %>% length) *
    (DECAY %>% length) *
    (TOTAL_PERC %>% length)
  totalOps %<>% `-`(totMatches)

  # Load up the odds frame
  odds.frame <- footballstats::odds.frame

  # Start looping the grid
  for (i in 1:(DAYS %>% length)) {
    for (j in 1:(GRID_PTS %>% length)) {
      for (k in 1:(GRID_BOUND %>% length)) {
        for (l in 1:(DECAY %>% length)) {
          for (m in 1:(TOTAL_PERC %>% length)) {

            # Check for any matched rows
            if (totMatches > 0) {
              check <- data.frame(
                day = DAYS[i],
                gridPoints = GRID_PTS[j],
                gridBoundary = GRID_BOUND[k],
                decay = DECAY[l],
                totalPercentage = TOTAL_PERC[m],
                type = types,
                stringsAsFactors = FALSE
              )

              # If there is a direct match then move onto the next iteration
              matched <- check %>%
                footballstats::get_grid_matches(
                  fullGrid = existing.topscore,
                  r = TRUE
                )

              if (matched) next
            }

            icount %<>% `+`(1)
            cat(' ## Analysing operation', icount, '/', totalOps, ' (Loading data first) \n')

            # Determine expected goals/ accuracy etc
            all.results <- total.metrics %>%
              footballstats::sub_metrics(
                colNames = colNames,
                GRIDS = GRIDS,
                odds.frame = odds.frame
              )

            # Prepare data - get the scales and scale results
            scaled.results <- all.results$data %>%
              mltools::scale_data()

            # Create plots + get feature metrics
            feat.metrics <- scaled.results$data %>%
              footballstats::create_plot(
                day = DAYS[i],
                gridPoints = GRID_PTS[j],
                gridBoundary= GRID_BOUND[k],
                decayFactor = DECAY[l],
                totalPer = TOTAL_PERC[m],
                savePlot = FALSE
              )

            # Initialise all methods
            allMethods <- list()

            # Build XGBoost model using CV
            if ("xgboost" %in% types) {
              startTime <- Sys.time()

              # Build and save XGBoost
              allMethods$xgb <- scaled.results$data %>%
                mltools::gen_xgb(
                  XGB = XGB
                )

              # Now calculate odds
              allMethods$xgb$totalStats$netWinnings <- sapply(
                X = allMethods$xgb$results,
                FUN = function(x) all.results$odds %>% footballstats::calculate_winnings(x)
              )

              endTime <- Sys.time()
              tDiff <- difftime(
                time1 = endTime,
                time2 = startTime
              ) %>% format
              cat(" XGBoost took :", tDiff, "\n")

              # Save the models
              if ("xgboost" %in% saveModels) { # nocov start
                xgModel <- allMethods$xgb$model
                xgScales <- scaled.results$scaler
                save(xgModel, file = modelDir %>% paste0("xgModel.rda"))
                save(xgScales, file = modelDir %>% paste0("xgScales.rda"))
              } # nocov end
            }

            # Build Neural network model using CV
            if ("neuralnetwork" %in% types) {
              startTime <- Sys.time()

              # Build and save NN
              allMethods$neuralnetwork <- scaled.results$data %>%
                mltools::gen_nn(
                  NN = NN,
                  logs = TRUE
                )

              # Now calculate odds
              allMethods$neuralnetwork$totalStats$netWinnings <- sapply(
                X = allMethods$neuralnetwork$results,
                FUN = function(x) odds.results %>% footballstats::calculate_winnings(x)
              )

              endTime <- Sys.time()
              tDiff <- difftime(
                time1 = endTime,
                time2 = startTime
              ) %>% format
              cat(" Neural Network took :", tDiff, "\n")

              # Save the models
              if ("neuralnetwork" %in% saveModels) { # nocov start
                nnModel <- allMethods$neuralnetwork$model
                nnScales <- scaled.results$scaler
                save(nnModel, file = modelDir %>% paste0("nnModel.rda"))
                save(nnScales, file = modelDir %>% paste0("nnScales.rda"))
              } # nocov end
            }

            # Write metrics to file
            if (optimizeModels) { # nocov start
              allMethods %>%
                footballstats::optimize_save_metrics(
                  resultsFile = resultsFile,
                  resultsDir = resultsDir
                )
            } # nocov end
          }
        }
      }
    }
  }

  if (!optimizeModels) return(allMethods)
}
