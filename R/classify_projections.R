#' @title Commentary Projection
#' @export


project_commentaries <- function(competitionID, seasonStarting, teamIDs) {

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

    # Get data frame of commentary metrics
    bFrame <- commentaryKeys %>%
      footballstats::get_av(
        commentaryNames = dataScales$commentaries %>%
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

    # Only take the average of the last 4 matches!
    if (bFrame %>% nrow %>% `<`(4)) next
    bFrame <- bFrame[1:4, ]

    # Calculate the average (and possible the standard deviation?)
    resList %<>% c(apply(bFrame, 2, mean) %>% list)
    resSds %<>% c(apply(bFrame, 2, sd) %>% `/`(3))
  }

  # Return a mini frame containing commentary information
  dataScales$commentaries %>%
    handle_projections(resList = resList) %>%
    return()
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
    handle_projections(resList = resList) %>%
    return()
}

#' @title Handle Projections
#'
#' @export


handle_projections <- function(frameNames, resList) {
  toFrame <- if (resList %>% length %>% `!=`(2)) {
    NA %>% rep(frameNames %>% length) %>% t
  } else {
    resList %>% purrr::flatten_dbl() %>% t
  }

  dF <- toFrame %>% data.frame(stringsAsFactors = FALSE)
  names(dF) <- frameNames
  dF %>% return()
}
