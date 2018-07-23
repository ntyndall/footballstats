#' @title Optimize Variables
#'
#' @export


optimize_variables <- function(total.metrics,
                               overwrite = FALSE,
                               DAYS = c(3, 4, 5),
                               GRID_PTS = c(2, 4, 6),
                               GRID_BOUND = c(0.1, 0.2),
                               DECAY = c(1, 5000),
                               TOTAL_PERC = seq(from = 0.0, to = 1.0, by = 1),
                               REP = 1,
                               THRESH = 0.18,
                               types = c("neuralnetwork", "xgboost")) {

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
    )[ , 1:5]

    # Get matches with existing data frame
    totMatches <- existing.topscore %>%
      footballstats::get_grid_matches(
        fullGrid = expand.grid(DAYS, GRID_PTS, GRID_BOUND, DECAY, TOTAL_PERC)
      )
  } else {
    totMatches <- 0
  }

  # Define neural network input list
  NN <- list(
    REP = REP,
    THRESH = THRESH
  )

  # Define XGBoost input list
  XGB <- list(
    ROUNDS = 50000,
    DEPTH = 10,
    ETA = 0.2,
    GAMMA = 2
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
                stringsAsFactors = FALSE
              )

              matched <- check %>%
                footballstats::get_grid_matches(
                  fullGrid = existing.metrics,
                  r = TRUE
                )

              if (matched) next
            }

            icount %<>% `+`(1)
            cat(' ## Analysing operation', icount, '/', totalOps, ' ')
            odds.results <- total.results <- data.frame(stringsAsFactors = FALSE)

            # Now loop over all of total.metrics
            for (drow in 2:(total.metrics %>% nrow)) {
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
                result.dat <- home.away.dat %>% footballstats::optimize_calculation(
                  day = DAYS[i],
                  gridPoints = GRID_PTS[j],
                  gridBoundary= GRID_BOUND[k],
                  decayFactor = DECAY[l],
                  til = current.row$til,
                  totalPer = TOTAL_PERC[m]
                )

                # Append positions on
                result.dat$`position.h` <- current.row$`position.h`
                result.dat$`position.a` <- current.row$`position.a`
                result.dat$res <- current.row$result
                total.results %<>% rbind(result.dat)

                # Make sure there is a match, if not then set as NA
                matchingIndex <- current.row$id %>% `==`(odds.frame$matchID)
                odds.results %<>% rbind(
                  if (matchingIndex %>% any) {
                    odds.frame[matchingIndex %>% which, ]
                  } else {
                    data.frame(
                      matchID = current.row$id,
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

            # Replace NA's with 0 for now.
            total.results[total.results %>% is.na] <- 0.0

            # With complete data set, get scaling parameters
            dataScales <- total.results %>%
              footballstats::get_scales()

            # Scale the data set
            scaled.results <- total.results %>%
              footballstats::scale_data(
                dataScales = dataScales
              )

            # Create plots + get feature metrics
            feat.metrics <- scaled.results %>%
              footballstats::create_plot(
                day = DAY[i],
                gridPoints = GRID_PTS[j],
                gridBoundary= GRID_BOUND[k],
                decayFactor = DECAY[l],
                totalPer = TOTAL_PERC[m],
                savePlot = FALSE
              )

            # Calculate folds for all methods
            myFolds <- caret::createFolds(
              y = total.results$res,
              k = 10,
              list = TRUE,
              returnTrain = FALSE
            )

            # Create fold data information
            FOLD_DATA <- list(
              FOLDS = myFolds,
              NUM = 10,
              PER = 7
            )

            # Build XGBoost model using CV
            startTime <- Sys.time()
            xgb <- total.results %>%
              footballstats::method_xgboost(
                odds.results = odds.results,
                FOLD_DATA = FOLD_DATA,
                XGB = XGB
              )
            endTime <- Sys.time()
            tDiff <- difftime(
              time1 = endTime,
              time2 = startTime
            ) %>% format
            cat(" XGBoost took :", tDiff, "\n")

            # Build neural network using CV
            startTime <- Sys.time()
            nn <- scaled.results %>%
              footballstats::neural_network(
                odds.results = odds.results,
                FOLD_DATA = FOLD_DATA,
                NN = NN,
                LOGS = FALSE
              )
            endTime <- Sys.time()
            tDiff <- difftime(
              time1 = endTime,
              time2 = startTime
            ) %>% format
            cat(" Nueral Network took :", tDiff, "\n")

            # Store the best result + output to screen
            nnCurrentResult <- nn$totAcc %>% mean
            xgbCurrentResult <- xgb$totAcc %>% mean
            biggest <- nnCurrentResult %>% max(xgbCurrentResult)

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
            allMethods <- list(xgb, nn)
            for (z in 1:(types %>% length)) {
              # Get average sensitivities
              sensD <- allMethods[[z]]$totD %>% mean
              sensL <- allMethods[[z]]$totL %>% mean
              sensW <- allMethods[[z]]$totW %>% mean

              # Store all results in a data frame
              topscore.frame <- data.frame(
                day = DAYS[i],
                gridPoints = GRID_PTS[j],
                gridBoundary = GRID_BOUND[k],
                decay = DECAY[l],
                totalPercentage = TOTAL_PERC[m],
                profit = profit, # Still need to calculate this
                `nn.threshold` = THRESH,
                `accuracy` = allMethods[[z]]$totAcc %>% mean,
                `accuracy.sd` = allMethods[[z]]$totAcc %>% stats::sd(),
                `accuracy.min` = allMethods[[z]]$totAcc %>% min,
                `accuracy.max` = allMethods[[z]]$totAcc %>% max,
                `sensitivity.D` = allMethods[[z]]$totD %>% mean,
                `sensitivity.D.min` = allMethods[[z]]$totD %>% min,
                `sensitivity.D.max` = allMethods[[z]]$totD %>% max,
                `sensitivity.L` = allMethods[[z]]$totL %>% mean,
                `sensitivity.L.min` = allMethods[[z]]$totL %>% min,
                `sensitivity.L.max` = allMethods[[z]]$totL %>% max,
                `sensitivity.W` = allMethods[[z]]$totW %>% mean,
                `sensitivity.W.min` = allMethods[[z]]$totW %>% min,
                `sensitivity.W.max` = allMethods[[z]]$totW %>% max,
                `sensitivity.sd` = c(sensD, sensL, sensW) %>% stats::sd(),
                stringsAsFactors = FALSE
              )

              # Make sure the results file exists and write header information
              scoreFile <- resultsDir %>% paste0(types[z], "_results.csv")
              if (scoreFile %>% file.exists %>% `!`()) topscore.frame %>% head_write(y = scoreFile)

              # Write results to files line by line
              topscore.frame %>% paste(collapse = ',') %>% write(file = scoreFile, append = T)
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
