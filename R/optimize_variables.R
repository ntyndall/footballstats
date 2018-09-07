#' @title Optimize Variables
#'
#' @export


optimize_variables <- function(total.metrics, GRIDS, optimizeModels = TRUE,
                               overwrite = FALSE, types = c("xgboost", "neuralnetwork"),
                               saveModels = c()) {

  # Create directory
  if (saveModels %>% length %>% `>`(0)) {
    modelDir <- getwd() %>% paste0("/mymodels/")
    if (modelDir %>% dir.exists %>% `!`()) modelDir %>% dir.create
  }

  # Only save models when requested
  if (optimizeModels) {
    if (saveModels %>% length %>% `>`(0)) {
      cat(" ## If you want to save any models, then set optimizeModels to FALSE! \n\n")
      saveModels <- c()
    }
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
  if (optimizeModels) {
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
  }

  # Define neural network input list
  NN <- list(
    REP = GRIDS$NN_REP,
    THRESH = GRIDS$NN_THRESH %>% max(0.3)
  )

  # Define XGBoost input list
  XGB <- list(
    ROUNDS = GRIDS$XG_ROUNDS,
    DEPTH = GRIDS$XG_DEPTH,
    ETA = GRIDS$XG_ETA,
    GAMMA = GRIDS$XG_GAMMA
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
            odds.results <- total.results <- data.frame(stringsAsFactors = FALSE)
            allMatchIDs <- c()

            # Set up a progress bar here
            pb <- utils::txtProgressBar(
              min = 0,
              max = total.metrics %>% nrow,
              style = 3
            )

            # Now loop over all of total.metrics
            for (drow in 2:(total.metrics %>% nrow)) {

              # Update the progress bar
              utils::setTxtProgressBar(
                pb = pb,
                value = drow
              )

              current.row <- total.metrics[drow, ]
              smaller.metrics <- total.metrics[1:(drow - 1), ]

              # Subset smaller subset for logical matches
              haMatches <- list(
                hh = smaller.metrics$localID %>% `==`(current.row$localID),
                ah = smaller.metrics$awayID %>% `==`(current.row$localID),
                ha = smaller.metrics$localID %>% `==`(current.row$awayID),
                aa = smaller.metrics$awayID %>% `==`(current.row$awayID)
              )

              # Number of rows of each type
              allSums <- haMatches %>% purrr::map(sum)

              if (allSums %>% purrr::map(function(x) x > DAYS[i]) %>% purrr::flatten_lgl() %>% all) {

                # Separating function
                sep_dat <- function(x, d, s) return(x[(s - d + 1):(x %>% nrow), ])

                # Get grid values here
                home.away.dat <- rbind(
                  smaller.metrics %>% subset(haMatches$hh) %>% sep_dat(d = DAYS[i], s = allSums$hh),
                  smaller.metrics %>% subset(haMatches$ah) %>% sep_dat(d = DAYS[i], s = allSums$ah),
                  smaller.metrics %>% subset(haMatches$ha) %>% sep_dat(d = DAYS[i], s = allSums$ha),
                  smaller.metrics %>% subset(haMatches$aa) %>% sep_dat(d = DAYS[i], s = allSums$aa)
                )

                # do calculations here
                result.dat <- home.away.dat %>%
                  footballstats::optimize_calculation(
                    day = DAYS[i],
                    gridPoints = GRID_PTS[j],
                    gridBoundary= GRID_BOUND[k],
                    decayFactor = DECAY[l],
                    til = current.row$til,
                    totalPer = TOTAL_PERC[m]
                  )

                # Append positions on
                result.dat$`position.h` <- current.row$`position.h` %>% `/`(current.row$til)
                result.dat$`position.a` <- current.row$`position.a` %>% `/`(current.row$til)
                result.dat$res <- current.row$result
                total.results %<>% rbind(result.dat)
                allMatchIDs %<>% c(current.row$matchID)

                # Make sure there is a match, if not then set as NA
                matchingIndex <- current.row$matchID %>% `==`(odds.frame$matchID)
                odds.results %<>% rbind(
                  if (matchingIndex %>% any) {
                    odds.frame[matchingIndex %>% which, ]
                  } else {
                    data.frame(
                      matchID = current.row$matchID,
                      homewin = NA,
                      draw = NA,
                      awaywin = NA,
                      stringsAsFactors = FALSE
                    )
                  }
                )
              } else {
                next
              }
            }

            # Match up matchIDs with odds frame
            new.odds <- odds.frame[allMatchIDs %>% match(odds.frame$matchID), ]

            # Remove NA's from matchIDs
            allMatchIDs %<>%
              `[`(total.results %>% stats::complete.cases())

            # Prepare data - get the scales and scale results
            scaled.results <- total.results %>%
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
                FUN = function(x) new.odds %>% footballstats::calculate_winnings(x)
              )

              endTime <- Sys.time()
              tDiff <- difftime(
                time1 = endTime,
                time2 = startTime
              ) %>% format
              cat(" XGBoost took :", tDiff, "\n")

              # Save the models
              if ("xgboost" %in% saveModels) {
                xgModel <- allMethods$xgb$model
                xgScales <- scaled.results$scaler
                save(xgModel, file = modelDir %>% paste0("xgModel.rda"))
                save(xgScales, file = modelDir %>% paste0("xgScales.rda"))
              }
            }

            # Build Neural network model using CV
            if ("neuralnetwork" %in% types) {
              startTime <- Sys.time()

              # Build and save NN
              allMethods$neuralnetwork <- scaled.results$data %>%
                mltools::gen_nn(
                  NN = NN
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
              if ("neuralnetwork" %in% saveModels) {
                nnModel <- allMethods$neuralnetwork$model
                nnScales <- scaled.results$scaler
                save(nn, file = modelDir %>% paste0("nnModel.rda"))
                save(nnScales, file = modelDir %>% paste0("nnScales.rda"))
              }
            }

            if (optimizeModels) {
              # What is the best result
              biggest <- sapply(
                X = 1:(allMethods %>% length),
                FUN = function(x) allMethods[[x]]$totalStats$totAcc %>% mean
              ) %>%
                max

              # Print to screen the best result so far
              if (biggest %>% `>`(bestResult)) {
                cat(
                  ' \n   -> New best result of :', biggest,
                  'from :', bestResult, '\n'
                )
                bestResult <- biggest
              }

              # Write headers function
              head_write <- function(x, y) x %>% names %>% paste(collapse = ",") %>% write(file = y)

              # Put the different methods into a list
              for (z in 1:(types %>% length)) {
                # Rename list object
                myStats <- allMethods[[z]]$totalStats

                # Get average sensitivities
                sensD <- myStats$totD %>% mean
                sensL <- myStats$totL %>% mean
                sensW <- myStats$totW %>% mean

                # Store all results in a data frame
                topscore.frame <- data.frame(
                  day = DAYS[i],
                  gridPoints = GRID_PTS[j],
                  gridBoundary = GRID_BOUND[k],
                  decay = DECAY[l],
                  totalPercentage = TOTAL_PERC[m],
                  type = types[z],
                  `accuracy` = myStats$totAcc %>% mean,
                  `profit` = myStats$netWinnings %>% mean,
                  `profit.sd` = myStats$netWinnings %>% stats::sd(),
                  `profit.min` = myStats$netWinnings %>% min,
                  `profit.max` = myStats$netWinnings %>% max,
                  `accuracy.sd` = myStats$totAcc %>% stats::sd(),
                  `accuracy.min` = myStats$totAcc %>% min,
                  `accuracy.max` = myStats$totAcc %>% max,
                  `sensitivity.D` = myStats$totD %>% mean,
                  `sensitivity.D.min` = myStats$totD %>% min,
                  `sensitivity.D.max` = myStats$totD %>% max,
                  `sensitivity.L` = myStats$totL %>% mean,
                  `sensitivity.L.min` = myStats$totL %>% min,
                  `sensitivity.L.max` = myStats$totL %>% max,
                  `sensitivity.W` = myStats$totW %>% mean,
                  `sensitivity.W.min` = myStats$totW %>% min,
                  `sensitivity.W.max` = myStats$totW %>% max,
                  `sensitivity.sd` = c(sensD, sensL, sensW) %>% stats::sd(),
                  stringsAsFactors = FALSE
                )

                # Make sure the results file exists and write header information
                if (resultsFile %>% file.exists %>% `!`()) topscore.frame %>% head_write(y = resultsFile)

                # Write results to files line by line
                topscore.frame %>%
                  paste(collapse = ',') %>%
                  write(
                    file = resultsFile,
                    append = TRUE
                  )
              }

              # Make sure the metrics file exists and write header information
              featureFile <- resultsDir %>% paste0("features.csv")
              if (featureFile %>% file.exists %>% `!`()) feat.metrics %>% head_write(y = featureFile)

              for (feat in 1:(feat.metrics %>% nrow)) {
                feat.metrics[feat, ] %>% paste(collapse = ',') %>% write(file = featureFile, append = T)
              }
            }
          }
        }
      }
    }
  }

  if (!optimizeModels) return(allMethods)
}
