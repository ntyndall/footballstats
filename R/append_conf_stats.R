#' @title Append confusion matrix
#'
#' @export


append_conf_stats <- function(totalStats, new.odds, Actual.score, Predicted.score, LOGS = FALSE) {

  # Build a table of results
  resultTable <- table(Actual.score, Predicted.score)
  rt <- caret::confusionMatrix(data = resultTable)

  # Refine stats here from the confusion matrix
  oStats <- rt$overall[c('Accuracy', 'AccuracyLower', 'AccuracyUpper')] %>% as.double
  oSens <-  rt$byClass[1:3, 'Sensitivity'] %>% as.double

  # Calculate odd information if it exists..
  if (new.odds %>% nrow %>% `>`(0)) {
    correctlyMatched <- Actual.score %>% `==`(Predicted.score)
    new.odds %<>% subset(correctlyMatched)
    winners <- Actual.score[correctlyMatched] %>% as.character

    # For each of win / draw / lose
    resultType <- c("W", "D", "L")
    totalRight <- totSum <- 0
    for (i in 1:3) {
      resultsMatched <- winners %>% `==`(resultType[i])
      totSum %<>% `+`(
        if (resultsMatched %>% any) {
          new.odds[[i + 1]] %>%
            as.numeric %>%
            `[`(resultsMatched) %>%
            sum
        } else {
          0
        }
      )

      totalRight %<>% `+`(resultsMatched %>% sum)

    }

    # Lost money is just the sum of incorrect guesses
    lostMoney <- Actual.score %>%
      length %>%
      `-`(totalRight)

    # Net earnings
    netWinnings <- totSum %>%
      `-`(lostMoney)

    # Bind to list
    totalStats$netWinnings %<>% c(totSum %>% `-`(lostMoney))
  }

  # Any dud results, just skip the whole lot
  if (oStats %>% is.na %>% any || oSens %>% is.na %>% any) {
    return(totalStats)
  } else {
    # Append list results on
    totalStats$totAcc %<>% c(oStats[1])
    totalStats$totAccL %<>% c(oStats[2])
    totalStats$totAccU %<>% c(oStats[3])
    totalStats$totD %<>% c(oSens[1])
    totalStats$totL %<>% c(oSens[2])
    totalStats$totW %<>% c(oSens[3])
  }

  return(totalStats)
}

#' @title Initialise Confusion Matrix List
#'
#' @export


init_conf_stats <- function() {
  return(
    list(
      totAcc = c(),
      totAccL = c(),
      totAccU = c(),
      totD = c(),
      totL = c(),
      totW = c(),
      netWinnings = c()
    )
  )
}
