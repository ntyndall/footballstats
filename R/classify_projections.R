#' @title Commentary Projection
#'
#' @description A function that tries to project the values of the commentary
#'  information by getting the raw commentary data of previous matches for the
#'  two teamIDs. Variables are used, such as how many commentaries to look back on
#'  to make the projection. Then the positions of those teams when they played the matches
#'  are taken to try and normalise the commentary information as best as possible, this
#'  squeezes data closer to try and see how well they actually played while trying to lift
#'  the constraint of teams being physically better.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{csmt_commentary:{comp_id}:{season}:{match_id}:{team_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param teamIDs A character vector of length two, containing the home team
#'  and away team in that order.
#' @param matchDate A character string of the matchDate API form, i.e. of
#'  the form dd.mm.yyyy
#' @param matchID A character string that represents the current matchID
#'  under investigation.
#'
#' @return A data frame of projected commentary information based on the
#'  data found in redis.
#'
#' @import e1071
#'
#' @export


project_commentaries <- function(KEYS, teamIDs, matchDate, matchID) {

  # Initialise
  resSds <- resList <- weights <- c()
  commKey <- paste0("csmt_commentary:", KEYS$COMP, ":", KEYS$SEASON)

  # Get the commentary names
  commentaryNames <- footballstats::dataScales$nn$commentaries %>%
    strsplit(split = '[.]') %>%
    purrr::map(1) %>%
    purrr::flatten_chr() %>%
    unique

  # Get the positions of the two current teams!
  positions <- KEYS %>%
    footballstats::feat_position(
      matchID = matchID,
      teamIDs = teamIDs,
      matchDate = matchDate
    ) %>% as.integer

  # Loop over both teams
  for (j in 1:2) {
    allCommentaryKeys <- paste0(commKey, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (allCommentaryKeys %>% is.null) break else allCommentaryKeys %<>% as.character

    # If it does then continue on
    allCommentaryKeys <- KEYS %>% footballstats::order_commentaries(
      commentaryKeys = allCommentaryKeys
    ) %>% rev

    # Get all the relative positions
    matchIDs <- allCommentaryKeys %>%
      footballstats::flatt(y = 3)

    commentaryKeys <- allCommentaryKeys
    bigger <- matchID %>% `>`(matchIDs)
    if (bigger %>% sum %>% `>`(4)) commentaryKeys %<>% `[`(bigger %>% which %>% `[`(1:4)) else next

    # Team IDs will ALWAYS be c(home, away)
    HAvec <- j %>% magrittr::mod(2)

    totalPositions <- data.frame(stringsAsFactors = FALSE)
    newMatchIDs <- commentaryKeys %>%
      footballstats::flatt(y = 3) %>%
      as.integer
    calcPos <- data.frame()
    concede <- goals <- 0
    totOppData <- c()

    for (k in 1:(allCommentaryKeys %>% length)) {
      if (totOppData %>% length %>% `==`(KEYS$DAYS)) next

      # Get the other information from the csm key
      oppResults <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchIDs[k]) %>%
        rredis::redisHMGet(fields = c('formatted_date', 'localteam_id', 'visitorteam_id', 'localteam_score', 'visitorteam_score')) %>%
        as.character

      if ("NULL" %in% oppResults) next

      twoIDs <- oppResults[c(2, 3)]
      if (oppResults %>% `==`(teamIDs[j]) %>% which %>% `==`(2)) {
        oth <- oppResults[3]
        goals %<>% `+`(oppResults[4] %>% as.integer)
        concede %<>% `+`(oppResults[5] %>% as.integer)
      } else {
        oth <- oppResults[2]
        goals %<>% `+`(oppResults[5] %>% as.integer)
        concede %<>% `+`(oppResults[4] %>% as.integer)
      }
      oppFrame <- footballstats::feat_position(
        KEYS = KEYS,
        matchID = matchIDs[k],
        teamIDs = c(oth, teamIDs[j])
      )

      calcPos %<>% rbind(oppFrame)
      totOppData %<>% c(oppFrame$position.h)
    }





    # Look for home games!!
    if (j == 1) {
      scoreVenue <- concedeVenue <- c()
      for (k in 1:(allCommentaryKeys %>% length)) {
        if (scoreVenue %>% length %>% `==`(KEYS$DAYS)) next
        someStats <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchIDs[k]) %>%
          rredis::redisHMGet(fields = c('localteam_id', 'localteam_score', 'visitorteam_score')) %>%
          as.character %>%
          as.integer
        if (teamIDs[j] == someStats[1]) {
          scoreVenue %<>% c(someStats[2])
          concedeVenue %<>% c(someStats[3])
        }
      }
    } else {
      scoreVenue <- concedeVenue <- c()
      for (k in 1:(allCommentaryKeys %>% length)) {
        if (scoreVenue %>% length %>% `==`(KEYS$DAYS)) next
        someStats <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchIDs[k]) %>%
          rredis::redisHMGet(fields = c('visitorteam_id', 'visitorteam_score', 'localteam_score')) %>%
          as.character

        if ("NULL" %in% someStats) next else someStats %<>% as.integer
        if (teamIDs[j] == someStats[1]) {
          scoreVenue %<>% c(someStats[2])
          concedeVenue %<>% c(someStats[3])
        }
      }
    }

    if (scoreVenue %>% length %>% `!=`(KEYS$DAYS)) next









    # Get data frame of commentary metrics
    comMetrics <- commentaryKeys %>%
      footballstats::commentary_frame(
        commentaryNames = commentaryNames
      )

    # Get number of rows for commentary frame
    comRows <- comMetrics %>% nrow

    # Check the commentary feature NA list (as database will not always have complete set)
    naCount <- sapply(comMetrics, function(x) x %>% is.na %>% sum) %>% as.integer
    thresh <- comRows %>% `/`(4)
    if (`>`(naCount, thresh) %>% any) next
    comMetrics[comMetrics %>% is.na] <- 0

    # Only take the average of the last 4 matches!
    if (comMetrics %>% nrow %>% `<`(KEYS$DAYS)) next
    comMetrics <- comMetrics[1:KEYS$DAYS, ]
    totOppData %<>% `[`(c(1:KEYS$DAYS))

    # Get mean and standard deviation for all metrics
    comMean <- apply(comMetrics, 2, mean)
    clinical <- goals %>% `/`(comMetrics$shots_ongoal %>% sum) %>% `*`(100)
    concede <- concede %>% `/`(oShots) %>% `*`(100)

    # Calculate the average (and possible the standard deviation?)
    resList %<>% c(
      c(comMean, totOppData %>% mean, clinical, concede, scoreVenue %>% sum, concedeVenue %>% sum) %>%
        as.double %>% list
    )
  }

  # Define the commentary frame names here
  allNames <- c(
    commentaryNames %>% paste0('.h'),
    c('strength.h', 'clinical.h', 'defensive.h', 'scored.h', 'concede.h'),
    commentaryNames %>% paste0('.a'),
    c('strength.a', 'clinical.a', 'defensive.a', 'scored.a', 'concede.a')
  )

  # Return a mini frame containing commentary information
  commentaryFrame <- allNames %>%
    footballstats::handle_projections(
      resList = resList
    )

  # Return the frame back
  commentaryFrame %>% return()
}


#' @title Form Projection
#'
#' @description A function that projects the form of the two teams to the
#'  build up of this match.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{csmt_commentary:{comp_id}:{season}:{match_id}:{team_id}}}
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param teamIDs A character vector of length two, containing the home team
#'  and away team in that order.
#' @param currentID An integer ID that is the current matchID being analysed
#'
#' @return A data frame that contains two columns, `form.h` and `form.a`
#' @export


project_form <- function(KEYS, teamIDs, currentID) {

  resList <- forms <- c()
  for (j in 1:2) {
    commentaryKeys <- paste0('csmt_commentary:', KEYS$COMP, ":", KEYS$SEASON, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break else commentaryKeys %<>% as.character

    # If it does then continue on
    commentaryKeys <- KEYS %>% footballstats::order_commentaries(
      commentaryKeys = commentaryKeys
    )

    # Get match IDs
    matchIDs <- commentaryKeys %>%
      footballstats::flatt(y = 3) %>%
      as.integer

    # Count how many are greater than the currentID
    matchIDs %<>% subset(currentID > matchIDs)

    # Needs to be KEYS$DAYS or more long
    if (matchIDs %>% length %>% `<`(KEYS$DAYS)) next

    # Construct matchData like obect
    csmIDs <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchIDs)
    cLen <- csmIDs %>% length
    matchData <- data.frame(stringsAsFactors = FALSE)

    # Loop over all match info
    for (k in (cLen - 3):cLen) {
      matchData %<>% rbind(
        csmIDs[k] %>%
        rredis::redisHGetAll() %>%
        as.data.frame(stringsAsFactors = FALSE)
      )
    }

    # Get average and append form on
    forms <- footballstats::form_from_matchdata(
      teamID = teamIDs[j],
      matchData = matchData
    )

    # Calculate the currentForm metric
    resList %<>% c(forms %>% list)
  }

  # Return a mini frame containing form information
  return(
    c('form.h', 'form.a') %>%
      footballstats::handle_projections(resList = resList)
  )
}


#' @title Handle Projections
#'
#' @description A function that is used to handle empty data
#'  when trying to project the commentaries or form etc. It
#'  creates a data frame of consistent format with the data
#'  frame names provided.
#'
#' @param frameNames A character vector of names to assign to
#'  the data frame columns.
#' @param resList A list of length two, which contains the home
#'  team and away team data in that order.
#'
#' @return A data frame with column names defined by \code{frameNames}.
#'
#' @export


handle_projections <- function(frameNames, resList) {
  toFrame <- if (resList %>% length %>% `!=`(2)) {
    NA %>% rep(frameNames %>% length) %>% t
  } else {
    #if (frameNames == c('form.h', 'form.a')) {
    #  resList %>% purrr::flatten_chr() %>% t
    #} else {
      resList %>% purrr::flatten_dbl() %>% t
    #}
  }

  dF <- toFrame %>% data.frame(stringsAsFactors = FALSE)
  names(dF) <- frameNames
  dF %>% return()
}
