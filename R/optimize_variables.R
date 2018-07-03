#' @title Optimize Variables
#'
#' @export


optimize_variables <- function(total.metrics,
                               DAYS = c(3, 4, 5),
                               GRID_PTS = c(2, 4, 6, 8),
                               GRID_BOUND = c(0.05, 0.1, 0.15),
                               DECAY = c(0.5, 1, 1.5, 2, Inf),
                               TOTAL_PERC = seq(from = 0.0, to = 1.0, by = 0.25),
                               REP = 1,
                               THRESH = 0.01) {

  # Define neural network input list
  NN <- list(
    REP = REP,
    THRESH = THRESH
  )

  # Initialise values for generating and tracking results
  topscore.frame <- data.frame(stringsAsFactors = FALSE)
  bestResult <- icount <- 0
  totalOps <- (DAYS %>% length) *
    (GRID_PTS %>% length) *
    (GRID_BOUND %>% length) *
    (DECAY %>% length) *
    (TOTAL_PERC %>% length)

  # Start looping the grid
  for (i in 1:(DAYS %>% length)) {
    day <- DAYS[i]
    for (j in 1:(GRID_PTS %>% length)) {
      for (k in 1:(GRID_BOUND %>% length)) {
        for (l in 1:(DECAY %>% length)) {
          for (m in 1:(TOTAL_PERC %>% length)) {
            icount %<>% `+`(1)
            cat(' ## Analysing operation', icount, '/', totalOps, '\n')
            total.results <- data.frame(stringsAsFactors = FALSE)

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
                result.dat <- home.away.dat %>% optimize_calculation(
                  day = day,
                  gridPoints = GRID_PTS[j],
                  gridBoundary= GRID_BOUND[k],
                  decayFactor = DECAY[l],
                  til = current.row$til,
                  totalPer = TOTAL_PERC[m]
                )

                result.dat$`position.h` <- current.row$`position.h`
                result.dat$`position.a` <- current.row$`position.a`
                result.dat$res <- current.row$result
                total.results %<>% rbind(result.dat)
              } else {
                next
              }
            }
          }

          # Replace NA's with 0 for now.
          total.results[total.results %>% is.na] <- 0.0

          # With complete data set, build NN..
          dataScales <- total.results %>% footballstats::get_scales()
          scaled.results <- total.results %>% footballstats::scale_data(dataScales = dataScales)
          nn <- scaled.results %>% footballstats::neural_network(NN = NN, LOGS = T)

          # Store the best result + output to screen
          currentResult <- nn$totAcc %>% mean
          if (currentResult > bestResult) {
            cat(' ## New best result of :', currentResult, 'from :', bestResult, '\n')
            bestResult <- currentResult
          }

          # Store all results in a data frame
          topscore.frame %<>% rbind(
            data.frame(
              day = DAYS[i],
              gridPoints = GRID_PTS[j],
              gridBoundary = GRID_BOUND[k],
              decay = DECAY[l],
              totalPercentage = TOTAL_PERC[m],
              accuracy = currentResult,
              `accuracy.sd` = nn$totAcc %>% stats::sd(),
              `sensitivity.D` = nn$totD %>% mean,
              `sensitivity.L` = nn$totL %>% mean,
              `sensitivity.W` = nn$totW %>% mean,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }

}
