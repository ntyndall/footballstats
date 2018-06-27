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
    day <- DAYS[i]
    for (j in 1:(GRID_PTS %>% length)) {
      for (k in 1:(GRID_BOUND %>% length)) {
        for (l in 1:(DECAY %>% length)) {
          # Now loop over all of total.metrics
          for (drow in 2:(total.metrics %>% nrow)) {
            current.row <- total.metrics[drow, ]
            smaller.metrics <- total.metrics[1:(drow - 1), ]

            home <- current.row$localID
            away <- current.row$awayID

            hHomeMatched <- smaller.metrics$localID %>% `==`(home)
            aHomeMatched <- smaller.metrics$awayID %>% `==`(home)
            hAwayMatched <- smaller.metrics$localID %>% `==`(away)
            aAwayMatched <- smaller.metrics$awayID %>% `==`(away)

            # Number of rows of each type
            hHomeSum <- hHomeMatched %>% sum
            aHomeSum <- aHomeMatched %>% sum
            hAwaySum <- hAwayMatched %>% sum
            aAwaySum <- aAwayMatched %>% sum

            if (hHomeSum %>% `>=`(day) && aHomeSum %>% `>=`(day) && hAwaySum %>% `>=`(day) && aAwaySum %>% `>=`(day)) {

              # Get grid values here
              h.home.dat <- smaller.metrics %>% subset(hHomeMatched)
              h.home.dat <- h.home.dat[(hHomeSum - day + 1):(h.home.dat %>% nrow), ]

              a.home.dat <- smaller.metrics %>% subset(aHomeMatched)
              a.home.dat <- a.home.dat[(aHomeSum - day + 1):(a.home.dat %>% nrow), ]

              h.away.dat <- smaller.metrics %>% subset(hAwayMatched)
              h.away.dat <- h.away.dat[(hAwaySum - day + 1):(h.away.dat %>% nrow), ]

              a.away.dat <- smaller.metrics %>% subset(aAwayMatched)
              a.away.dat <- a.away.dat[(aAwaySum - day + 1):(a.away.dat %>% nrow), ]

              home.away.dat <- rbind(h.home.dat, a.home.dat, h.away.dat, a.away.dat)

              # do calculations here
              home.away.dat %>% footballstats::optimize_calculation(
                day = day,
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
