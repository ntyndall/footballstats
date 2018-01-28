#' @title Create weekly points
#'
#' @description MAKE SURE THE MATCH DATA IS RECREATED!!
#'
#' @export


create_table <- function(matchData) {

  # Get the competitionID first
  competitionID <- matchData$comp_id %>%
    unique

  # Get the unique season
  seasonStarting <- matchData$season %>%
    unique %>%
    strsplit(split = '/') %>%
    purrr::map(1) %>%
    purrr::flatten_chr()

  # SET the very first date if it doesn't exist!
  firstDate <- paste0('c_startDate:', competitionID, ':', seasonStarting)
  if (firstDate %>% rredis::redisExists() %>% `!`()) {
    firstElement <- matchData[1, ]$formatted_date %>%
      as.integer %>%
      as.character %>%
      charToRaw()
    firstDate %>% rredis::redisSet(value = firstElement)
  }

  # GET the starting date
  startDate <- firstDate %>%
    rredis::redisGet() %>%
    as.integer

  # Loop over all match data
  for (i in 1:(matchData %>% nrow)) {

    # Break match data down to individual slices
    slice <- matchData[i, ]
    matchID <- slice$id
    currentDate <- slice$formatted_date %>% as.integer
    teamIDs <- c(slice$localteam_id, slice$visitorteam_id)
    teamScore <- slice$ft_score %>% footballstats::prs_ftscore()
    scoreDiff <- teamScore[1] - teamScore[2]

    # Calculate the `on the fly` week number based on date away from first game!
    weekNum <- currentDate %>% `-`(startDate) %>% `/`(7) %>% floor %>% `+`(1)

    if (scoreDiff > 0) {
      hPts <- 3
      aPts <- 0
    } else if (scoreDiff == 0 ) {
      hPts <- aPts <- 1
    } else {
      hPts <- 0
      aPts <- 3
    }

    for (j in 1:2) {
      redisKey <- paste0('cwt_l:', competitionID, ':', seasonStarting, ':', weekNum, ':', teamIDs[j])

      pointsToAdd <- 'leagueMatchSet' %>% rredis::redisSAdd(
         element = paste0(matchID, ':', teamIDs[j]) %>% charToRaw()) %>%
        as.integer %>%
        as.logical

      # Create the key if it doesnt exist
      if (pointsToAdd && `!`(redisKey %>% rredis::redisExists())) {
        if (weekNum %>% `!=`(1)) {
          # Get last match info (may have to go back multiple weeks..)
          keepTrying <- TRUE
          starter <- 1
          while (keepTrying) {
            prevKey <- paste0('cwt_l:', competitionID, ':', seasonStarting, ':', weekNum - starter, ':', teamIDs[j])
            pRes <- prevKey %>%
              rredis::redisHGetAll() %>%
              lapply(as.integer)
            if (pRes %>% length %>% `>`(0)) {
              keepTrying <- FALSE
            } else {
              starter %<>% `+`(1)
            }
          }
        } else {
          pRes <- list(PTS = 0, GF = 0, GD = 0)
        }

        if (j == 1) {
          redisKey %>% rredis::redisHMSet(
            values = list(
              PTS = pRes$PTS %>% `+`(hPts) %>% as.character %>% charToRaw(),
              GF = pRes$GF %>% `+`(teamScore[1]) %>% as.character %>% charToRaw(),
              GD = pRes$GD %>% `+`(scoreDiff) %>% as.character %>% charToRaw()))
        } else {
          redisKey %>% rredis::redisHMSet(
            values = list(
              PTS = pRes$PTS %>% `+`(aPts) %>% as.character %>% charToRaw(),
              GF = pRes$GF %>% `+`(teamScore[2]) %>% as.character %>% charToRaw(),
              GD = pRes$GD %>% `+`(-scoreDiff) %>% as.character %>% charToRaw()))
        }
      }
    }
  }
}

#' @title Weekly Positions
#' @export

weekly_positions <- function(competitionID, seasonStarting) {

  # Get all possible keys
  redisKeys <- paste0('cwt_l:', competitionID, ':', seasonStarting, '*') %>%
    rredis::redisKeys()

  # Get the unique weeks to loop over
  weeks <- redisKeys %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer

  # Determine unique weeks and other boundaries
  uniqKeys <- weeks %>% unique %>% sort
  uniqLen <- uniqKeys %>% length
  uniqMin <- uniqKeys %>% min

  # Loop over all the unique keys..
  for (i in 1:uniqLen) {

    # Get subkeys
    subKeys <- redisKeys %>%
      subset(
        paste0(':', uniqKeys[i], ':') %>%
          grepl(redisKeys)
        )

    # There could be non-zero weeks..
    if (subKeys %>% identical(character(0))) next

    # Get the teamIDs
    teamIDs <- subKeys %>%
      strsplit(split = ':') %>%
      purrr::map(5) %>%
      purrr::flatten_chr()

    subKeyLen <- subKeys %>% length

    singleWeek <- data.frame(stringsAsFactors = FALSE)
    for (j in 1:subKeyLen) {
      # At this point I need to order them by something!
      subFrame <- subKeys[j] %>%
        rredis::redisHGetAll() %>%
        lapply(as.integer) %>%
        data.frame(stringsAsFactors = FALSE)

      subFrame$teamID <- teamIDs[j]
      # build the dataframe
      singleWeek %<>% rbind(subFrame)
    }
    # order frame by points.. gd.. then gf...
    singleWeek <- singleWeek[
      order(
        singleWeek$PTS,
        singleWeek$GD,
        singleWeek$GF,
        decreasing = TRUE
      ), ]

    position <- c(1:(singleWeek %>% nrow)) %>%
      as.character %>%
      sapply(charToRaw)
    names(position) <- singleWeek$teamID

    # Push list of positions to the cw_pl hashmap ...
    paste0('cw_pl:', competitionID, ':', seasonStarting, ':', uniqKeys[i]) %>%
      rredis::redisHMSet(values = position)
  }

}
