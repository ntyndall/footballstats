#' @title Commentary Projection
#' @export


project_commentaries <- function(competitionID, seasonStarting, teamIDs, matchDate, KEYS) {

  sVar <- if (KEYS$STAND %>% is.null) 0.3 else KEYS$STAND

  resSds <- resList <- weights <- c()
  for (j in 1:2) {
    commentaryKeys <- paste0('cmt_commentary:', competitionID, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break

    # If it does then continue on
    commentaryKeys %<>% as.character %>%
      footballstats::ord_keys(
        competitionID = competitionID,
        seasonStarting = seasonStarting
      ) %>% rev

    # --- WIP
    matchIDs <- commentaryKeys %>% strsplit(split = ':') %>% purrr::map(3) %>% purrr::flatten_chr()
    totalPositions <- data.frame(stringsAsFactors = FALSE)
    for (k in 1:(commentaryKeys %>% length)) {
      otherTeam <- paste0('cmt_commentary:', competitionID, ':', matchIDs[k], ':*') %>%
        rredis::redisKeys()

      otherTeam %<>% strsplit(split = ':') %>% purrr::map(4) %>% purrr::flatten_chr()
      otherTeam %<>% subset(otherTeam != teamIDs[j])

      positions <- footballstats::feat_position(
        competitionID = competitionID,
        seasonStarting = seasonStarting,
        matchID = matchIDs[k],
        teamIDs = c(teamIDs[j], otherTeam)
      )
      totalPositions %<>% rbind(positions)
    }
    # --- WIP

    # Get data frame of commentary metrics
    bFrame <- commentaryKeys %>%
      footballstats::get_av(
        commentaryNames = footballstats::dataScales$commentaries %>%
          strsplit(split = '[.]') %>%
          purrr::map(1) %>%
          purrr::flatten_chr() %>%
          unique
      )

    # Check the commentary feature NA list (as database will not always have complete set)
    naCount <- sapply(bFrame, function(x) x %>% is.na %>% sum) %>% as.integer
    thresh <- bFrame %>% nrow %>% `/`(4)
    if (`>`(naCount, thresh) %>% any) next
    bFrame[bFrame %>% is.na] <- 0

    # Get the extremeties first
    minss <- apply(bFrame, 2, min) %>% as.numeric
    maxss <- apply(bFrame, 2, max) %>% as.numeric

    # Only take the average of the last 4 matches!
    if (bFrame %>% nrow %>% `<`(4)) next
    bFrame <- bFrame[1:4, ]

    # Adjust bFrame
    tNew <- data.frame(stringsAsFactors = FALSE)
    facts <- totalPositions$position.h %>% `/`(totalPositions$position.a) %>% `^`(sVar)

    for (k in 1:(bFrame %>% nrow)) {
      new <- bFrame[k, ] %>% `*`(facts[k]) %>% as.numeric
      tNew %<>% rbind(new %>% pmax(minss) %>% pmin(maxss) %>% as.data.frame %>% t)
    }
    names(tNew) <- names(bFrame)
    bFrame <- tNew

    # Calculate the average (and possible the standard deviation?)
    resList %<>% c(apply(bFrame, 2, mean) %>% list)
    resSds %<>% c(apply(bFrame, 2, stats::sd) %>% `/`(3))
  }

  # I need to get teams playing positions
  #######
  startDate <- paste0('c_startDate:', competitionID, ':', seasonStarting) %>%
    rredis::redisGet() %>%
    as.integer

  # Get the current date
  currentDate <- matchDate %>%
    as.Date(format = '%d.%m.%Y') %>%
    as.integer

  # Convert to week number
  weekNum <- currentDate %>%
    `-`(startDate) %>%
    `/`(7) %>%
    floor %>%
    `+`(1)

  # Get the last known position of the two teams
  weekKeys <- paste0('cw_pl:', competitionID, ':', seasonStarting, ':*') %>%
    rredis::redisKeys() %>%
    footballstats::get_weeks()

  prevWeek <- weekKeys %>%
    `[`(weekNum %>%
          `-`(weekKeys) %>%
          abs %>% which.min
    )

  positions <- paste0('cw_pl:', competitionID, ':', seasonStarting, ':', prevWeek) %>%
    rredis::redisHGetAll() %>%
    lapply(as.integer)

  adjust <- list(
    home = positions[[teamIDs[1]]],
    away = positions[[teamIDs[2]]],
    min = minss,
    max = maxss,
    sVar = sVar
  )
  #####
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


project_form <- function(competitionID, seasonStarting, teamIDs) {

  resList <- forms <- c()
  for (j in 1:2) {
    commentaryKeys <- paste0('cmt_commentary:', competitionID, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break

    # If it does then continue on
    commentaryKeys %<>%
      as.character %>%
      footballstats::ord_keys(
        competitionID = competitionID,
        seasonStarting = seasonStarting
      )

    # Get match IDs
    matchIDs <- commentaryKeys %>%
      strsplit(split = ':') %>%
      purrr::map(3) %>%
      purrr::flatten_chr()

    # Needs to be 3 or more long
    if (matchIDs %>% length %>% `<`(3)) next

    # Construct matchData like obect
    csmIDs <- paste0('csm:', competitionID, ':', seasonStarting, ':', matchIDs)
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
      adj <- adjust$home %>% `/`(adjust$away) %>% `^`(adjust$sVar)
      resList[[1]] %<>% as.numeric %>% `*`(adj) %>% pmax(adjust$min) %>% pmin(adjust$max)
      adj <- adjust$away %>% `/`(adjust$home) %>% `^`(adjust$sVar)
      resList[[2]] %<>% as.numeric %>% `*`(adj) %>% pmax(adjust$min) %>% pmin(adjust$max)
    }
    resList %>% purrr::flatten_dbl() %>% t
  }

  dF <- toFrame %>% data.frame(stringsAsFactors = FALSE)
  names(dF) <- frameNames
  dF %>% return()
}
