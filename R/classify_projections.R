#' @title Commentary Projection
#' @export


project_commentaries <- function(competitionID, seasonStarting, teamIDs) {

  # Get start date
  startDate <- 'c_startDate:1204' %>% rredis::redisGet() %>% as.integer

  resSds <- resList <- weights <- c()
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

    # Calculate the average (and possible the standard deviation?)
    resList %<>% c(apply(bFrame, 2, mean) %>% list)
    resSds %<>% c(apply(bFrame, 2, sd) %>% `/`(3))
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


#' @title Project Position
#' @export


project_position <- function() {
  # Date from matchID
  matchIDs <- commentaryKeys %>%
    strsplit(split = ':') %>%
    purrr::map(3) %>%
    purrr::flatten_chr()

  # Get general match info i.e. the date
  # get_dates()
  subWeight <- c()
  for (k in 1:(matchIDs %>% length)) {
    matchInfo <- paste0('csm:1204:2017:', matchIDs[k]) %>%
      rredis::redisHMGet(
        fields = c('formatted_date', 'visitorteam_id', 'localteam_id')
      ) %>%
      as.character

    # Pick the other ID!
    truth <- matchInfo == teamIDs[j]
    otherID <- matchInfo[c(FALSE, !truth[c(2:3)])]

    # Get the current date
    currentDate <- matchInfo[1] %>%
      as.Date(format = '%d.%m.%Y') %>%
      as.integer
    weekNum <- currentDate %>% `-`(startDate) %>% `/`(7) %>% floor %>% `+`(1)

    # Get the positions from the week being investigated
    positions <- paste0('cw_pl:1204:', weekNum) %>% rredis::redisHGetAll()
    #if (positions %>% is.null) next
    position <- positions[[teamIDs[j]]] %>% as.integer
    positionAgainst <- positions[[otherID]] %>% as.i
    nteger

    # The league size
    if (k == 1) lSize <- positions %>% length

    # If less then lower to normalize!
    subWeight %<>% c( if (position > positionAgainst) {
      1 %>% `-`(position %>% `-`(positionAgainst) %>% min(10) %>% `/`(20))
    } else {
      positionAgainst %>% `-`(position) %>% min(10) %>% `/`(20) %>% `+`(1)
    })
  }
  weights %<>% c(subWeight %>% list)
}
