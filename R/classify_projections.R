#' @title Commentary Projection
#' @export


project_commentaries <- function(KEYS, teamIDs, matchDate, matchID) {

  # Initialise
  resSds <- resList <- weights <- c()
  commKey <- paste0('cmt_commentary:', KEYS$COMP)

  # Get the commentary names
  commentaryNames <- footballstats::dataScales$commentaries %>%
    strsplit(split = '[.]') %>%
    purrr::map(1) %>%
    purrr::flatten_chr() %>%
    unique

  # Loop over both teams
  for (j in 1:2) {
    commentaryKeys <- paste0(commKey, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break

    # If it does then continue on
    commentaryKeys %<>% as.character %>%
      footballstats::ord_keys(
        KEYS = KEYS
      ) %>% rev

    # Get all the relative positions
    matchIDs <- commentaryKeys %>%
      footballstats::flatt(y = 3)

    totalPositions <- data.frame(stringsAsFactors = FALSE)
    for (k in 1:(commentaryKeys %>% length)) {

      # Get the other team from the matchID
      bothIDs <- paste0(commKey, ':', matchIDs[k], ':*') %>%
        rredis::redisKeys() %>%
        footballstats::flatt(y = 4)

      # Get the positions from
      positions <- KEYS %>% footballstats::feat_position(
        matchID = matchIDs[k],
        teamIDs = bothIDs
      )

      # Bind to a total data frame
      totalPositions %<>% rbind(positions)
    }

    # Get data frame of commentary metrics
    comMetrics <- commentaryKeys %>%
      footballstats::get_av(
        commentaryNames = commentaryNames
      )

    # Get number of rows for commentary frame
    comRows <- comMetrics %>% nrow

    # Check the commentary feature NA list (as database will not always have complete set)
    naCount <- sapply(comMetrics, function(x) x %>% is.na %>% sum) %>% as.integer
    thresh <- comRows %>% `/`(4)
    if (`>`(naCount, thresh) %>% any) next
    comMetrics[comMetrics %>% is.na] <- 0

    # Get the extremeties first
    minVals <- apply(comMetrics, 2, min) %>% as.numeric
    maxVals <- apply(comMetrics, 2, max) %>% as.numeric

    # Only take the average of the last 4 matches!
    if (comMetrics %>% nrow %>% `<`(KEYS$DAYS)) next
    comMetrics <- comMetrics[1:KEYS$DAYS, ]

    # Adjust bFrame
    newMetrics <- data.frame(stringsAsFactors = FALSE)
    facts <- totalPositions$position.h %>%
      `/`(totalPositions$position.a) %>%
      `^`(KEYS$STAND)

    for (k in 1:KEYS$DAYS) {
      # Adjust score by the factor
      adjustedScore <- comMetrics[k, ] %>%
        `*`(facts[k]) %>%
        as.numeric

      # Make sure factors dont exceed the bounds
      newMetrics %<>% rbind(
        adjustedScore %>%
          pmax(minVals) %>%
          pmin(maxVals) %>%
          as.data.frame %>%
          t
      )
    }
    names(newMetrics) <- names(comMetrics)

    # Calculate the average (and possible the standard deviation?)
    resList %<>% c(apply(newMetrics, 2, mean) %>% list)
    resSds %<>% c(apply(newMetrics, 2, stats::sd) %>% `/`(3))
  }

  # Get the positions of the two current teams
  positions <- KEYS %>%
    footballstats::feat_position(
      matchID = matchID,
      teamIDs = teamIDs,
      matchDate = matchDate
    )

  # Create an adjust object to tweak the resList
  adjust <- list(
    home = positions$position.h,
    away = positions$position.a,
    min = minVals,
    max = maxVals,
    sVar = KEYS$STAND
  )

  # Return a mini frame containing commentary information
  commentaryFrame <- dataScales$commentaries %>%
    footballstats::handle_projections(
      resList = resList,
      adjust = adjust
    )

  # Return the frame back
  commentaryFrame %>% return()
}


#' @title Commentary Projection
#' @export


project_form <- function(KEYS, teamIDs) {

  resList <- forms <- c()
  for (j in 1:2) {
    commentaryKeys <- paste0('cmt_commentary:', KEYS$COMP, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break

    # If it does then continue on
    commentaryKeys %<>%
      as.character %>%
      footballstats::ord_keys(
        KEYS = KEYS
      )

    # Get match IDs
    matchIDs <- commentaryKeys %>%
      footballstats::flatt(y = 3)

    # Needs to be 3 or more long
    if (matchIDs %>% length %>% `<`(KEYS$DAYS)) next

    # Construct matchData like obect
    csmIDs <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchIDs)
    cLen <- csmIDs %>% length
    matchData <- data.frame(stringsAsFactors = FALSE)

    # Loop over all match info
    for (k in (cLen - 2):cLen) {
      matchData %<>% rbind(
        csmIDs[k] %>%
        rredis::redisHGetAll() %>%
        as.data.frame
      )
    }

    # Get average and append form on
    forms %<>% footballstats::get_frm(
      teamID = teamIDs[j],
      matchData = matchData
    )

    # Calculate the currentForm metric
    resList %<>% c(forms %>% list)
  }

  # Return a mini frame containing form information
  c('form.h', 'form.a') %>%
    footballstats::handle_projections(resList = resList) %>%
    return()
}

#' @title Handle Projections
#'
#' @export


handle_projections <- function(frameNames, resList, adjust = NULL) {
  toFrame <- if (resList %>% length %>% `!=`(2)) {
    NA %>% rep(frameNames %>% length) %>% t
  } else {
    if (adjust %>% is.null %>% `!`()) {

      # Get fraction between home and away
      fr <- adjust$home %>% `/`(adjust$away)
      for (i in 1:2) {
        if (i == 2) fr %<>% `^`(-1) # Inverse for away / home
        resList[[i]] %<>%
          as.numeric %>%
          `*`(fr %>% `^`(adjust$sVar)) %>%
          pmax(adjust$min) %>%
          pmin(adjust$max)
      }
    }
    resList %>% purrr::flatten_dbl() %>% t
  }

  dF <- toFrame %>% data.frame(stringsAsFactors = FALSE)
  names(dF) <- frameNames
  dF %>% return()
}
