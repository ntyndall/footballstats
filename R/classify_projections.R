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
#'     \item{\strong{[HASH]} :: \code{cmt_commentary:{comp_id}:{match_id}:{team_id}}}
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
  commKey <- paste0('cmt_commentary:', KEYS$COMP)
  myMetrics <- footballstats::cIntervals
  mySVM <- footballstats::allsvms

  # Get the commentary names
  commentaryNames <- footballstats::dataScales$commentaries %>%
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
    commentaryKeys <- paste0(commKey, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break else commentaryKeys %<>% as.character

    # If it does then continue on
    commentaryKeys <- KEYS %>% footballstats::order_commentaries(
      commentaryKeys = commentaryKeys
    ) %>% rev

    # Get all the relative positions
    matchIDs <- commentaryKeys %>%
      footballstats::flatt(y = 3)

    # Team IDs will ALWAYS be c(home, away)
    HAvec <- j %>% mod(2)

    if (j == 2) positions %<>% rev

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

    # Get mean and standard deviation for all metrics
    comMean <- apply(comMetrics, 2, mean)
    comSD <- apply(comMetrics, 2, stats::sd)

    # for each metric need to adjust value (bucket the mean) and then
    #positionInt <- c(0, 5, 10, 15, 20, Inf)
    totalPreds <- c()
    for (k in 1:(commentaryNames %>% length)) {
      curName <- commentaryNames[k]
      #testVar <- comMean[[curName]] %>% findInterval(myMetrics[[curName]]) %>% as.factor

      myData <- data.frame(
        one = comMean[[curName]],
        two = comSD[[curName]],
        three = positions[1],
        four = positions[2],
        five = HAvec,
        stringsAsFactors = FALSE
      )
      names(myData) <- c(paste0(curName, c('_mean', '_sd')), 'currentPos', 'otherPos', 'homeaway')
      newValue <- stats::predict(mySVM[[KEYS$COMP %>% as.character]][[curName]], myData) %>%
        as.integer

      # Now I just need to multiply it back up!!
      #cMet <- myMetrics[[curName]]
      #midPoint <- cMet[2] %>% `-`(cMet[1]) %>% `/`(2)
      #finalVal <- cMet[newValue] %>% `+`(midPoint)
      totalPreds %<>% c(newValue)
    }

    # Calculate the average (and possible the standard deviation?)
    resList %<>% c(totalPreds %>% list)
  }

  # Get the positions of the two current teams
  positions <- KEYS %>%
    footballstats::feat_position(
      matchID = matchID,
      teamIDs = teamIDs,
      matchDate = matchDate
    )

  # Return a mini frame containing commentary information
  commentaryFrame <- footballstats::dataScales$commentaries %>%
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
#'     \item{\strong{[HASH]} :: \code{cmt_commentary:{comp_id}:{match_id}:{team_id}}}
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param teamIDs A character vector of length two, containing the home team
#'  and away team in that order.
#'
#' @return A data frame that contains two columns, `form.h` and `form.a`
#' @export


project_form <- function(KEYS, teamIDs) {

  resList <- forms <- c()
  for (j in 1:2) {
    commentaryKeys <- paste0('cmt_commentary:', KEYS$COMP, ':*:', teamIDs[j]) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break else commentaryKeys %<>% as.character

    # If it does then continue on
    KEYS %>% footballstats::order_commentaries(
      commentaryKeys = commentaryKeys
    )

    # Get match IDs
    matchIDs <- commentaryKeys %>%
      footballstats::flatt(y = 3)

    # Needs to be KEYS$DAYS or more long
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
    forms <- footballstats::form_from_matchdata(
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
#' @description A function that is used to handle empty data
#'  when trying to project the commentaries or form etc. It
#'  creates a data frame of consistent format with the data
#'  frame names provided. It can also make adjustments to
#'  the results based on the adjust param.
#'
#' @param frameNames A character vector of names to assign to
#'  the data frame columns.
#' @param resList A list of length two, which contains the home
#'  team and away team data in that order.
#' @param adjust A list object to handle the commentary data so it
#'  is tweaked based on the variable parameters.
#'
#' @return A data frame with column names defined by \code{frameNames}.
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
