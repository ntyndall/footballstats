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

  # Get basic metrics back
  mets <- lapply(
    X = 1:2,
    FUN = function(x) {
      # Select correct frame
      if (x == 1) {
        cFrame <- frameList[[1]]
        oFrame <- frameList[[2]]
      } else {
        oFrame <- frameList[[3]]
        cFrame <- frameList[[4]]
      }

      # Get metric data
      analyse.data <- cFrame %>%
        footballstats::create_feature_data(
          type = if (x == 1) "h" else "a"
        )

      # Get position list
      positions <- list(
        posH = cFrame$position.h,
        posA = cFrame$position.a
      )

      # Just home or away depending on team
      agg.data <- positions %>%
        footballstats::optimize_get_data(
          cData = analyse.data,
          gridPoints = gridPoints,
          mygrid = mygrid,
          boundaries = boundaries,
          decayFactor = decayFactor
        )

      # Create combination of home and away
      if (totalPer > 0.01) {
        ha.dat <- footballstats::optimize_sort_ha(
          homeDat = cFrame,
          awayDat = oFrame,
          gridPoints = gridPoints,
          mygrid = mygrid,
          boundaries = boundaries,
          ha = if (x == 1) "h" else "a",
          decayFactor = decayFactor
        )
        agg.data <- ha.dat %>% `*`(totalPer) %>% `+`(agg.data * (1 - totalPer))
      }

      # Return from lapply
      return(agg.data)
    }
  )

  # Return row of data frame here
  return(mets %>% purrr::reduce(cbind))
}

#' @title Optimize Get Dat
#'
#' @export


optimize_get_data <- function(positions, cData, gridPoints, mygrid, boundaries, decayFactor) {
  uu <- positions %>%
    footballstats::optimize_positiongrid(
      gridPoints = gridPoints,
      mygrid = mygrid,
      boundaries = boundaries
    )

  # Calculate decay Factors
  decayFactors <- cData %>%
    nrow %>%
    footballstats::optimize_decay(
      decay = decayFactor
    ) %>%
    rev

  # Apply the factors and summarise data
  summarisedRes <- cData %>%
    apply(MARGIN = 2, FUN = function(x) x * uu) %>%
    apply(MARGIN = 2, FUN = function(x) x * decayFactors) %>%
    apply(MARGIN = 2, FUN = sum) %>%
    t %>%
    data.frame

  return(summarisedRes)
}

#' @title Optimize Sort HA
#'
#' @export


optimize_sort_ha <- function(homeDat, awayDat, gridPoints, mygrid, boundaries, ha = 'h', decayFactor) {
  allNames <- homeDat %>% names

  # Remove til
  homeDat$til <- NULL

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
    result = awayDat$result %>% footballstats::flip_res(),
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

  # Order the data frame
  orderDat <- rbind(homeDat, newAway)
  orderDat <- orderDat[orderDat$date %>% order, ]

  # Get metrics back
  analyse.data <- orderDat %>%
    footballstats::create_feature_data()

  # Rename if looking at away team
  # (it is fudged to consider home but needs changed back now)
  if (ha == 'a') {
    names(analyse.data) <- analyse.data %>%
      names %>%
      strsplit(split = '[.]') %>%
      purrr::map(1) %>%
      purrr::flatten_chr() %>%
      paste0('.a')
  }

  # Adjust with factors
  newDat <- list(
    posH = orderDat$position.h,
    posA = orderDat$position.a
  ) %>%
    footballstats::optimize_get_data(
      cDat = analyse.data,
      gridPoints = gridPoints,
      mygrid = mygrid,
      boundaries = boundaries,
      decayFactor = decayFactor
  )

  return(newDat)
}

#' @title Flip Result
#'
#' @export


flip_res <- function(x) {
  # Readjust results...
  cRes <- x
  newRes <- c()
  for (i in 1:(cRes %>% length)) {
    newRes %<>% c(if (cRes[i] == 'W') 'L' else if (cRes[i] == 'L') 'W' else 'D')
  }
  return(newRes)
}

#' @title Create Feature Data
#'
#' @export


create_feature_data <- function(cFrame, type = 'h') {

  # Make sure to get the right information
  goals <- if (type == 'h') cFrame$localScore else cFrame$awayScore
  ftRes <- if (type == 'h') cFrame$result else cFrame$result %>% footballstats::flip_res()
  oGoals <- if (type == 'h') cFrame$awayScore else cFrame$localScore
  oType <- if (type == 'h') 'a' else 'h'


  featFrame <- data.frame(
    xg = goals %>% as.integer,
    form = ftRes %>% footballstats::formint_2(),
    clinical = goals %>% footballstats::take_ratio(y = cFrame[[paste0('shots_ongoal.', type)]]),
    defensive = oGoals %>% footballstats::take_ratio(y = cFrame[[paste0('shots_ongoal.', oType)]]),
    shotacc = cFrame[[paste0('shots_ongoal.', type)]] %>% footballstats::take_ratio(y = cFrame[[paste0('shots_total.', type)]]),
    shotrate = cFrame[[paste0('shots_total.', type)]] %>% footballstats::take_ratio(y = cFrame[[paste0('possesiontime.', type)]]),
    stringsAsFactors = FALSE
  )

  names(featFrame) <- featFrame %>% names %>% paste0('.', type)
  return(featFrame)
}

#' @title Form 2 Int
#'
#' @export


formint_2 <- function(oldForms, winPoints = 2, drawPoints = 1, losePoints = 0) {
  newForms <- c()
  for (p in 1:(oldForms %>% length)) {
    newForms %<>% c(if (oldForms[p] == 'W') 2 else if (oldForms[p] == 'D') 1 else 0)
  }
  return(newForms)
}


#' @title Take Ratio
#'
#' @export


take_ratio <- function(x, y) {

  # Replace NaN with 0
  repl_nan <- function(x) {
    anyNan <- x %>% is.nan
    if (anyNan %>% any) x[anyNan] <- 0.0
    return(x)
  }

  # Calculate ratio
  return(x %>% as.integer %>% `/`(y %>% as.integer) %>% pmin(1) %>% repl_nan())
}
