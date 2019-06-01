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
          type = if (x == 1) "home" else "away"
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
          cFrame = cFrame,
          oFrame = oFrame,
          gridPoints = gridPoints,
          mygrid = mygrid,
          boundaries = boundaries,
          ha = if (x == 1) "home" else "away",
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


optimize_sort_ha <- function(cFrame, oFrame, gridPoints, mygrid, boundaries, ha = 'h', decayFactor) {

  d.sets <- list(cFrame, oFrame)

  oFrame %>% names -> nuNames

  # Find the consistent team (Brighton) and map it to home
  oFrame$zzz.til <- cFrame$zzz.til <- NULL
  # Just list all the rules out first!

  # Get standard columns
  nFrame <- oFrame  %>% `[`(oFrame %>% names %>% grepl(pattern = "^zzz\\."))

  # Map home to away ---
  nFrame1 <- oFrame %>% `[`(oFrame %>% names %>% grepl(pattern = "^home\\."))
  names(nFrame1) <- nFrame1 %>% names %>% gsub(pattern = "^home\\.", replacement = "away.")

  nFrame2 <- oFrame %>% `[`(oFrame %>% names %>% grepl(pattern = "\\.h$"))
  names(nFrame2) <- nFrame2 %>% names %>% gsub(pattern = "\\.h$", replacement = ".a")

  # Map away to home ---
  nFrame3 <- oFrame %>% `[`(oFrame %>% names %>% grepl(pattern = "^away\\."))
  names(nFrame3) <- nFrame3 %>% names %>% gsub(pattern = "^away\\.", replacement = "home.")

  nFrame4 <- oFrame %>% `[`(oFrame %>% names %>% grepl(pattern = "\\.a$"))
  names(nFrame4) <- nFrame4 %>% names %>% gsub(pattern = "\\.a$", replacement = ".h")

  nFrame %<>% cbind(
    nFrame1, nFrame2, nFrame3, nFrame4
  )

  # Flip the result of zzz.result!!!
  nFrame$zzz.result %<>% footballstats::flip_res()

  # Now bind them both on!
  cFrame %<>% rbind(nFrame)

  # Order the data frame
  cFrame <- cFrame[cFrame$zzz.date %>% order, ]

  # Get metrics back
  analyse.data <- cFrame %>%
    footballstats::create_feature_data(
      type = ha
    )

  # Adjust with factors
  newDat <- list(
    posH = cFrame$position.h,
    posA = cFrame$position.a
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
  return(
    lapply(
      X = x,
      FUN = function(y) if (y == "W") "L" else if (y == "L") "W" else "D"
    ) %>%
      purrr::flatten_chr()
  )
}

#' @title Create Feature Data
#'
#' @export


create_feature_data <- function(cFrame, type = 'home') {

  # Get the matching score
  oType <- if (type == "home") "away" else "home"
  goals <- cFrame[[paste0(type, ".score")]]
  ftRes <- if (type == 'home') cFrame$zzz.result else cFrame$zzz.result %>% footballstats::flip_res()
  oGoals <- cFrame[[paste0(oType, ".score")]]

  # Get convinceability
  if (type == 'home') {
    gd <- cFrame$home.score %>% as.integer %>% `-`(cFrame$away.score %>% as.integer)
  } else {
    gd <- cFrame$away.score %>% as.integer %>% `-`(cFrame$home.score %>% as.integer)
  }

  convince <- sapply(
    X = gd,
    FUN = function(x) {
      if (x <= -3) {
        1
      } else if (x == -2) {
        2
      } else if (x == -1) {
        3
      } else if (x == 0) {
        4
      } else if (x == 1) {
        5
      } else if (x == 2) {
        6
      } else if (x >= 3) {
        7
      }
    }
  )

  featFrame <- data.frame(
    xg = goals %>% as.integer,
    form = ftRes %>% footballstats::form_to_int(),
    clinical = goals %>% footballstats::take_ratio(y = cFrame %>% `[[`(paste0(type, ".ontarget"))),
    defensive = oGoals %>% footballstats::take_ratio(y = cFrame %>% `[[`(paste0(oType, ".ontarget"))),
    shotacc = cFrame %>% `[[`(paste0(type, ".ontarget")) %>% footballstats::take_ratio(y = cFrame %>% `[[`(paste0(type, ".shots"))),
    convince = convince,
    stringsAsFactors = FALSE
  )

  names(featFrame) <- paste0(type, ".", featFrame %>% names)
  return(featFrame)
}

#' @title Form 2 Int
#'
#' @export


form_to_int <- function(oldForms, winPoints = 2, drawPoints = 1, losePoints = 0) {
  return(
    lapply(
      X = oldForms,
      FUN = function(x) if (x == "W") winPoints else if (x == "D") drawPoints else losePoints
    ) %>%
      purrr::flatten_dbl()
  )
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
