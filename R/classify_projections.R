#' @title Commentary Projection
#' @export


project_commentaries <- function(competitionID, seasonStarting, teamIDs) {

  resList <- c()
  for (j in 1:2) {
    commentaryKeys <- paste0('cmt_commentary:', competitionID, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break

    # If it does then continue on
    commentaryKeys %<>% as.character %>%
      footballstats::ord_keys(
        competitionID = competitionID,
        seasonStarting = seasonStarting)

    # Get data frame of commentary metrics
    bFrame <- commentaryKeys %>%
      footballstats::get_av(
        commentaryNames = dataScales$commentaries)

    # Check the commentary feature NA list (as database will not always have complete set)
    naCount <- sapply(bFrame, function(x) x %>% is.na %>% sum) %>% as.integer
    thresh <- bFrame %>% nrow %>% `/`(4)
    if (`>`(naCount, thresh) %>% any) next
    bFrame[bFrame %>% is.na] <- 0

    # Need to get the weights from matchID probably
    #matchWeights <- commentaryKeys %>% footballstats::match_weights()
    # Multiply each row of bFrame with matchWeights

    # Calculate the average (and possible the standard deviation?)
    resList %<>% c(apply(bFrame, 2, mean) %>% list)
    #sds <- apply(bFrame, 2, sd) %>% `/`(3)
  }

  #
  commentary <- if (resList %>% length %>% `!=`(2)) {
    NA %>% rep(dataScales$commentaries %>% length) %>% t
  } else {
    resList[[1]] %>% `-`(resList[[2]]) %>% t
  }

  dF <- commentary %>% data.frame(stringsAsFactors = FALSE)
  names(dF) <- dataScales$commentaries
  dF %>% return()
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
    commentaryKeys %<>% as.character %>%
      footballstats::ord_keys(
        competitionID = competitionID,
        seasonStarting = seasonStarting)

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
      matchData = matchData)

    # Calculate the currentForm metric
    resList %<>% c(forms %>% list)
  }

  #
  form <- if (resList %>% length %>% `!=`(2)) {
    NA  %>% t
  } else {
    resList[[1]] %>% `-`(resList[[2]]) %>% t
  }

  dF <- form %>% data.frame(stringsAsFactors = FALSE)
  names(dF) <- 'form'
  dF %>% return()
}

#' @title convince
#' @export

project_convince <- function(competitionID, seasonStarting, teamIDs) {

  resList <- c()
  for (j in 1:2) {
    commentaryKeys <- paste0('cmt_commentary:', competitionID, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break

    # If it does then continue on
    commentaryKeys %<>% as.character %>%
      footballstats::ord_keys(
        competitionID = competitionID,
        seasonStarting = seasonStarting)

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
          as.data.frame()
      )
    }

    # team ref
    ref <- data.frame(
      l = matchData$localteam_id,
      a = matchData$visitorteam_id,
      stringsAsFactors = FALSE)

    totConv <- 0
    for (i in 1:(ref %>% nrow)) {
      conv <- matchData$ft_score[i] %>%
        as.character %>%
        footballstats::prs_ftscore()
      ind <- ref[i, ] %>% `==`(teamIDs[j]) %>% which
      totConv %<>% `+`(if (ind %>% `==`(1)) `+`(conv[1] - conv[2]) else `-`(conv[1] - conv[2]))
    }
    totConv %<>% `/`(3)

    # Calculate the currentForm metric
    resList %<>% c(totConv %>% list)
  }

  #
  convince <- if (resList %>% length %>% `!=`(2)) {
    NA  %>% t
  } else {
    resList[[1]] %>% `-`(resList[[2]]) %>% t
  }

  dF <- convince %>% data.frame(stringsAsFactors = FALSE)
  names(dF) <- 'convince'
  dF %>% return()
}
