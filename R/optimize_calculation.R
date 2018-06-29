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
  if (gridPoints > 0) {
    boundaries <- seq(
      from = 1,
      to = til + 1,
      by = til / gridPoints
    )

    # Grid of constants
    mygrid <- c(
      seq(
        from = 1 - gridBoundary,
        by = gridBoundary / gridPoints * 2,
        length.out = gridPoints / 2
      ),
      seq(
        from = 1 + gridBoundary,
        by = - gridBoundary / gridPoints * 2,
        length.out = gridPoints / 2
      ) %>% rev
    )
  } else {
    mygrid <- 0.0
    boundaries <- c(1, til + 1)
  }

  # Loop over each type of data
  for (i in 1:(frameList %>% length)) {

    # Get the current frame
    cFrame <- frameList[[i]]

    # Set up conditions for each type first
    if (i == 1) {

      uu <- list(
        posH = cFrame$position.h,
        posA = cFrame$position.a
      ) %>%
      optimize_positiongrid(
        gridPoints = gridPoints,
        mygrid = mygrid,
        boundaries = boundaries
      )

      mets <- cFrame[ , c(10:15)] %>%
        optimize_rowwise(intervals = uu) %>%
        apply(MARGIN = 2, FUN = mean) %>%
        t %>%
        data.frame

      clinicalHome <- cFrame$localScore %>% as.integer %>% `/`(cFrame$shots_ongoal.h %>% as.integer)
      defensiveHome <- cFrame$awayScore %>% as.integer %>% `/`(cFrame$shots_ongoal.a %>% as.integer)
      dNan <- defensiveHome %>% is.nan
      cNan <- clinicalHome %>% is.nan
      if (dNan %>% any) defensiveHome[dNan] <- 0.0
      if (cNan %>% any) clinicalHome[cNan] <- 0.0
      mets %<>% cbind(
        data.frame(
          `clinical.h` = clinicalHome %>% mean,
          `defensive.a` = defensiveHome %>% mean,
          `xg.hh` = cFrame$localScore %>% as.integer %>% mean,
          stringsAsFactors = FALSE
        )
      )
    } else if (i == 2) {
      next
    } else if (i == 3) {
      next
    } else {

      uu <- list(
        posH = cFrame$position.a, # Looking at the away team so flip these for good reason!
        posA = cFrame$position.h
      ) %>%
        optimize_positiongrid(
          gridPoints = gridPoints,
          mygrid = mygrid,
          boundaries = boundaries
        )

      mets %<>% cbind(
        cFrame[ , c(16:21)] %>%
          optimize_rowwise(intervals = uu) %>%
          apply(MARGIN = 2, FUN = mean) %>%
          t %>%
          data.frame
      )

      defensiveAway <- cFrame$localScore %>% as.integer %>% `/`(cFrame$shots_ongoal.h %>% as.integer)
      clinicalAway <- cFrame$awayScore %>% as.integer %>% `/`(cFrame$shots_ongoal.a %>% as.integer)
      dNan <- defensiveAway %>% is.nan
      cNan <- clinicalAway %>% is.nan
      if (dNan %>% any) defensiveAway[dNan] <- 0.0
      if (cNan %>% any) clinicalAway[cNan] <- 0.0
      mets %<>% cbind(
        data.frame(
          `clinical.h` = clinicalAway %>% mean,
          `defensive.a` = defensiveAway %>% mean,
          `xg.aa` = cFrame$awayScore %>% as.integer %>% mean,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # Return row of data frame here
  return(mets)
}
