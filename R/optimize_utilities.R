#' @title Optimize Rowwise
#'
#' @export


optimize_rowwise <- function(frame, intervals) {
  firstNames <- frame %>% names
  mets <- apply(
    X = frame,
    MARGIN = 1,
    FUN = function(x) x %>% as.integer %>% `*`(intervals)
  ) %>%
    t %>%
    data.frame

  names(mets) <- firstNames
  return(mets)
}

#' @title Optimize Position Grid
#'
#' @export


optimize_positiongrid <- function(pos, gridPoints, mygrid, boundaries) {

  # pos is a list of posH, posA
  results <- c()
  for (i in 1:(pos$posH %>% length)) {
    h <- pos$posH[i]
    a <- pos$posA[i]
    res <- if (h < a) {
      mygrid %>% `[`(
        a %>% `-`(h) %>%
          findInterval(boundaries)
      )
    } else {
      mygrid %>% `[`(
        gridPoints %>% `+`(1) %>% `-`(
          a %>%
            `-`(h) %>%
            abs %>%
            findInterval(boundaries)
        )
      )
    }

    # If play-offs mean I can't find a position put it first or last
    if (res %>% length %>% `==`(0)) {
      res <- if (h > a) gridPoints[1] else gridPoints[gridPoints %>% length]
    }

    # Append results on
    results %<>% c(res)
  }
  return(results)
}

#' @title Get Grid Matches
#'
#' @export


get_grid_matches <- function(existing, fullGrid, r = FALSE) {
  # Make sure the grid names matches up
  names(fullGrid) <- names(existing)

  # Check for matches
  totMatches <- 0
  for (i in 1:(existing %>% nrow)) {
    res <- suppressMessages(
      expr = fullGrid %>%
        plyr::match_df(existing[i, ]) %>%
        nrow
    )
    if (res %>% `==`(1)) totMatches %<>% `+`(1)
  }

  # Return number of matches
  return(if (r) totMatches %>% `==`(existing %>% nrow) else totMatches)
}
