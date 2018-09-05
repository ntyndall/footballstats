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
