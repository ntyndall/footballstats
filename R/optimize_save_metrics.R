#' @title Optimize Save Metrics
#'
#' @export


optimize_save_metrics <- function(allMethods, resultsFile, resultsDir) { # nocov start
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
      accuracy = myStats$totAcc %>% mean,
      profit = myStats$netWinnings %>% mean,
      profit.sd = myStats$netWinnings %>% stats::sd(),
      profit.min = myStats$netWinnings %>% min,
      profit.max = myStats$netWinnings %>% max,
      accuracy.sd = myStats$totAcc %>% stats::sd(),
      accuracy.min = myStats$totAcc %>% min,
      accuracy.max = myStats$totAcc %>% max,
      sensitivity.D = myStats$totD %>% mean,
      sensitivity.D.min = myStats$totD %>% min,
      sensitivity.D.max = myStats$totD %>% max,
      sensitivity.L = myStats$totL %>% mean,
      sensitivity.L.min = myStats$totL %>% min,
      sensitivity.L.max = myStats$totL %>% max,
      sensitivity.W = myStats$totW %>% mean,
      sensitivity.W.min = myStats$totW %>% min,
      sensitivity.W.max = myStats$totW %>% max,
      sensitivity.sd = c(sensD, sensL, sensW) %>% stats::sd(),
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
} # nocov end
