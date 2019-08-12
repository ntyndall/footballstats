#' @title Calculate Winnings
#'
#' @export


calculate_winnings <- function(odds.results, winInfo) {

  # Subset the odds correctly
  new.odds <- odds.results[winInfo$foldint %>% sort, ]
  Actual.score <- winInfo$actual
  Predicted.score <- winInfo$predicted

  # Calculate odd information if it exists..
  if (new.odds %>% nrow %>% `>`(0)) {
    correctlyMatched <- Actual.score %>% `==`(Predicted.score)
    sub.odds <- new.odds %>% subset(correctlyMatched)
    winners <- Actual.score[correctlyMatched] %>% as.character

    # subset out any NA's here OR empty strings
    empt_or_na <- function(x) {
      res <- x %>% is.na %>% `|`(x %>% `==`(""))
      if (res %>% any) res %>% which else NULL
    }

    # Vectorize and check if any odds are missing
    toExclude <- lapply(
      X = c("homewin", "draw", "awaywin"),
      FUN = function (x) sub.odds[[x]] %>% empt_or_na()
    ) %>%
      purrr::flatten_int() %>%
      unique

    # If any are to be excluded then exclude them
    if (toExclude %>% length %>% `>`(0)) {
      lgcl <- TRUE %>% rep(sub.odds %>% nrow)
      lgcl[toExclude] <- FALSE
      sub.odds %<>% subset(lgcl)
      winners %<>% `[`(lgcl)
      exclusions <- toExclude %>% length
    } else {
      exclusions <- 0
    }

    # For each of win / draw / lose
    resultType <- c("W", "D", "L")
    totalRight <- totSum <- 0
    for (i in 1:3) {
      resultsMatched <- winners %>% `==`(resultType[i])
      totSum %<>% `+`(
        if (resultsMatched %>% any) {
          sub.odds[[i + 1]] %>%
            as.numeric %>%
            `[`(resultsMatched) %>%
            `-`(1) %>%
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
      `-`(exclusions) %>%
      `-`(totalRight)

    # Net earnings
    netWinnings <- totSum %>%
      `-`(lostMoney)

    # Bind to list
    return(totSum %>% `-`(lostMoney))
  }
}

####

calculate_winnings2 <- function(logicalVec, PRED_RESULTS, ACTUAL_RESULTS, odd.data) {
  notAnalysed <- 0
  odd.data$zzz.bet365Homewin %<>% as.numeric
  odd.data$zzz.bet365Draw %<>% as.numeric
  odd.data$zzz.bet365Awaywin %<>% as.numeric
  winnings <- c()
  for (i in 1:(logicalVec %>% length)) {
    # If TRUE then won some money here
    if (logicalVec[i]) {
      # Find out if it's a win / lose correct prediction
      if (PRED_RESULTS[i] == 'W') {
        winnings %<>% c(odd.data$zzz.bet365Homewin[i] %>% `-`(1))
      } else {
        # Must have lost / draw / lose?
        #if (test.metrics.scoring$zzz.bet365Awaywin[i] %>% `>`(2) %>% `&`(test.metrics.scoring$zzz.bet365Draw[i] %>% `>`(2))) {
        # What was the original bet?!?! Draw/ Lose?
        winnings %<>% c(
          if (ACTUAL_RESULTS[i] == 'D') {
            odd.data$zzz.bet365Draw[i] %>% `*`(0.5) %>% `-`(1)
          } else if (odd.data$zzz.bet365Awaywin[i] < 2.0) {
            0
          } else {
            odd.data$zzz.bet365Awaywin[i] %>% `*`(0.5) %>% `-`(1)
          }
        )
      }
    } else {
      winnings %<>% c(-1)
    }
  }
  return(winnings)
}
