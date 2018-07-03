#' @title Optimize Calculation
#'
#' @export


optimize_calculation <- function(home.away.dat, day, gridPoints, gridBoundary, decayFactor, totalPer, til) {

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

      # Just home
      first <- list(
        posH = cFrame$position.h,
        posA = cFrame$position.a
      ) %>%
        optimize_get_data(
          cData = cFrame[ , c(10:15)],
          gridPoints = gridPoints,
          mygrid = mygrid,
          boundaries = boundaries
        )

      first %<>% cbind(
        data.frame(
          `xg.h` = cFrame$localScore %>% as.integer %>% mean,
          `form.h` = cFrame$result %>% footballstats::form_to_int(),
          stringsAsFactors = FALSE
        )
      )

      # Home and away
      if (totalPer > 0.01) {
        haDat <- optimize_sort_ha(
          homeDat = cFrame,
          awayDat = frameList[[2]],
          gridPoints = gridPoints,
          mygrid = mygrid,
          boundaries = boundaries
        )
        mets <- haDat %>% `*`(totalPer) %>% `+`(first * (1 - totalPer))
      } else {
        mets <- first
      }

      clinicalHome <- cFrame$localScore %>% as.integer %>% `/`(cFrame$shots_ongoal.h %>% as.integer) %>% pmin(1)
      defensiveHome <- cFrame$awayScore %>% as.integer %>% `/`(cFrame$shots_ongoal.a %>% as.integer) %>% pmin(1)
      dNan <- defensiveHome %>% is.nan
      cNan <- clinicalHome %>% is.nan
      if (dNan %>% any) defensiveHome[dNan] <- 0.0
      if (cNan %>% any) clinicalHome[cNan] <- 0.0
      mets %<>% cbind(
        data.frame(
          `clinical.hh` = clinicalHome %>% mean,
          `defensive.ha` = defensiveHome %>% mean,
          stringsAsFactors = FALSE
        )
      )
    } else if (i == 2) {
      next
    } else if (i == 3) {
      next
    } else {
      # Just away
      second <- list(
        posH = cFrame$position.a,
        posA = cFrame$position.h
      ) %>%
        optimize_get_data(
          cData = cFrame[ , c(16:21)],
          gridPoints = gridPoints,
          mygrid = mygrid,
          boundaries = boundaries
        )

      # Readjust results...
      cRes <- cFrame$result
      newRes <- c()
      for (i in 1:(cRes %>% length)) {
        newRes %<>% c(if (cRes[i] == 'W') 'L' else if (cRes[i] == 'L') 'W' else 'D')
      }

      second %<>% cbind(
        data.frame(
          `xg.a` = cFrame$awayScore %>% as.integer %>% mean,
          `form.a` = newRes %>% footballstats::form_to_int(),
          stringsAsFactors = FALSE
        )
      )

      # Home and away
      if (totalPer > 0.01) {
        haDat <- optimize_sort_ha(
          homeDat = frameList[[3]],
          awayDat = cFrame,
          gridPoints = gridPoints,
          mygrid = mygrid,
          boundaries = boundaries,
          ha = 'a'
        )
        mets %<>% cbind(haDat %>% `*`(totalPer) %>% `+`(second * (1 - totalPer)))
      } else {
        mets %<>% cbind(second)
      }

      defensiveAway <- cFrame$localScore %>% as.integer %>% `/`(cFrame$shots_ongoal.h %>% as.integer) %>% pmin(1)
      clinicalAway <- cFrame$awayScore %>% as.integer %>% `/`(cFrame$shots_ongoal.a %>% as.integer) %>% pmin(1)
      dNan <- defensiveAway %>% is.nan
      cNan <- clinicalAway %>% is.nan
      if (dNan %>% any) defensiveAway[dNan] <- 0.0
      if (cNan %>% any) clinicalAway[cNan] <- 0.0
      mets %<>% cbind(
        data.frame(
          `clinical.ah` = clinicalAway %>% mean,
          `defensive.aa` = defensiveAway %>% mean,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # Return row of data frame here
  return(mets)
}

#' @title Optimize Get Dat
#'
#' @export


optimize_get_data <- function(positions, cData, gridPoints, mygrid, boundaries) {
  uu <- positions %>%
    optimize_positiongrid(
      gridPoints = gridPoints,
      mygrid = mygrid,
      boundaries = boundaries
    )

  mets <- cData %>%
    optimize_rowwise(intervals = uu) %>%
    apply(MARGIN = 2, FUN = mean) %>%
    t %>%
    data.frame
  return(mets)
}

#' @title Optimize Sort HA
#'
#' @export

optimize_sort_ha <- function(homeDat, awayDat, gridPoints, mygrid, boundaries, ha = 'h') {
  allNames <- homeDat %>% names

  # Readjust results...
  cRes <- awayDat$result
  newRes <- c()
  for (i in 1:(cRes %>% length)) {
    newRes %<>% c(if (cRes[i] == 'W') 'L' else if (cRes[i] == 'L') 'W' else 'D')
  }

  # Bluff home / away just to get the correct metrics! (as home away doesnt matter now)
  newAway <- data.frame(
    matchID = awayDat$matchID,
    date = awayDat$date,
    localName = awayDat$awayName,
    awayName = awayDat$localName,
    localID = awayDat$awayID,
    awayID = awayDat$localID,
    localScore = awayDat$awayScore,
    awayScore = awayDat$localScore,
    result = newRes,
    stringsAsFactors = FALSE
  )

  aStats <- awayDat[ ,c(16:21)]
  hStats <- awayDat[ ,c(10:15)]

  aNames <- aStats %>% names
  hNames <- hStats %>% names

  names(hStats) <- aNames
  names(aStats) <- hNames

  newAway %<>% cbind(
    hStats,
    aStats,
    data.frame(
      `position.h` = awayDat$position.a,
      `position.a` = awayDat$position.h,
      stringsAsFactors = FALSE
    )
  )

  orderDat <- rbind(homeDat, newAway)
  orderDat <- orderDat[orderDat$date %>% order, ]

  newDat <- list(
    posH = orderDat$position.h,
    posA = orderDat$position.a
  ) %>%
    optimize_get_data(
      cDat = orderDat[ , c(10:15)],
      gridPoints = gridPoints,
      mygrid = mygrid,
      boundaries = boundaries
  )

  other <- data.frame(
    orderDat$localScore %>% as.integer %>% mean,
    orderDat$result %>% footballstats::form_to_int(),
    stringsAsFactors = FALSE
  )
  names(other) <- paste0(c('xg.', 'form.', ha))
  newDat %<>% cbind(other)

  return(newDat)
}

