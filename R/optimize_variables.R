#' @title Optimize Variables
#'
#' @export


optimize_variables <- function(total.metrics) {

  # Define variables here
  DAYS <- c(3, 4, 5)
  GRID_PTS <- c(2, 4, 6, 8, 10)
  GRID_BOUND <- c(0.3, 0.4, 0.5, 0.6, 0.7)
  DECAY <- c(1)

  for (i in 1:(DAYS %>% length)) {
    for (j in 1:(GRID_PTS %>% length)) {
      for (k in 1:(GRID_BOUND %>% length)) {
        for (l in 1:(DECAY %>% length)) {
          # Now loop over all of total.metrics
          for (drow in 2:(total.metrics %>% nrow)) {
            current.row <- total.metrics[drow, ]
            smaller.metrics <- total.metrics[1:(drow - 1), ]

            home <- current.row$localID
            away <- current.row$awayID

            homeMatched <- smaller.metrics$localID %>% `==`(home)
            awayMatched <- smaller.metrics$awayID %>% `==`(away)

            homeSum <- homeMatched %>% sum
            awaySum <- awayMatched %>% sum

            if (homeMatched %>% sum %>% `>=`(DAYS[i]) && awayMatched %>% sum %>% `>`(DAYS[i])) {

              # do calculations here
              footballstats::optimize_calculation(
                day = DAYS[i],
                gridPoints = GRID_PTS[j],
                gridBoundary= GRID_BOUND[k],
                decayFactor = DECAY[l]
              )
            } else {
              next
            }
          }
        }
      }
    }
  }

}
