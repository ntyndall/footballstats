#' @title Optimize Calculation
#'
#' @export


optimize_calculation <- function(home.away.dat, day, gridPoints, gridBoundary, decayFactor, til) {

  # Split up data set again to do some analysis and build the features
  frameList <- list(
    hhome = home.away.dat[1:day, ],
    ahome = home.away.dat[(day + 1):(day * 2), ],
    haway = home.away.dat[((day * 2) + 1):(day * 3), ],
    aaway = home.away.dat[((day * 3) + 1):(day * 4), ]
  )

  # Set up boundaries and constants
  boundaries <- seq(
    from = 1,
    to = til + 1,
    by = til / gridPoints
  )

  # Grid of constants
  mygrid <- seq(
    from = 0.2,
    by = - (gridBoundary / gridPoints),
    length.out = gridPoints
  )

  # Loop over each type of data
  for (i in 1:(frameList %>% length)) {

    # Get the current frame
    cFrame <- frameList[[i]]

    # Set up conditions for each type first
    if (i == 1) {
      mets <- cFrame[ , c(10:15)]
      clinicalHome <- cFrame$localScore %>% as.integer %>% `/`(cFrame$shots_ongoal.h %>% as.integer)
      defensiveHome <- cFrame$awayScore %>% as.integer %>% `/`(cFrame$shots_ongoal.a %>% as.integer)
      dNan <- defensiveHome %>% is.nan
      cNan <- clinicalHome %>% is.nan
      if (dNan %>% any) defensiveHome[dNan] <- 0.0
      if (cNan %>% any) clinicalHome[cNan] <- 0.0
      mets %<>% cbind(
        data.frame(
          `clinical.h` = clinicalHome,
          `defensive.a` = defensiveHome,
          stringsAsFactors = FALSE
        )
      )
    } else if (i == 2) {
      next
    } else if (i == 3) {
      next
    } else {
      mets %<>% cbind(cFrame[ , c(16:21)])
      defensiveAway <- cFrame$localScore %>% as.integer %>% `/`(cFrame$shots_ongoal.h %>% as.integer)
      clinicalAway <- cFrame$awayScore %>% as.integer %>% `/`(cFrame$shots_ongoal.a %>% as.integer)
      dNan <- defensiveAway %>% is.nan
      cNan <- clinicalAway %>% is.nan
      if (dNan %>% any) defensiveAway[dNan] <- 0.0
      if (cNan %>% any) clinicalAway[cNan] <- 0.0
      mets %<>% cbind(
        data.frame(
          `clinical.h` = clinicalAway,
          `defensive.a` = defensiveAway,
          stringsAsFactors = FALSE
        )
      )
    }
   # inters <- findInterval(cFrame$position.h, boundaries)
  }


}
