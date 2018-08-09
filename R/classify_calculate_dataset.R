#' @title Calculate Data Set
#'
#' @description A function that takes current statistical data and combines
#'  a series of features into a data frame one at a time and returns a
#'  data set ready for analysis.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'   }
#'
#' @param matchData A dataframe containing all the match data.
#' @param logger A boolean value to indicate whether to print useful
#'  log information to screen.
#'
#' @return Returns a dataframe containing the column names of 'returnItems'
#'  plus a few other metrics.
#'
#' @export


calculate_data <- function(matchData, logger = FALSE) {

  # Only take these names
  allowedNames <- c(
    'shots_total', 'shots_ongoal', 'fouls', 'corners', 'possesiontime', 'yellowcards'
  )

  # Infer the season
  KEYS$SEASON <- matchData$season %>% footballstats::prs_season()
  mDat <- data.frame(stringsAsFactors = FALSE)

  # Data rows
  rowData <- matchData %>% nrow
  res <- c()

  # Set up progress bar
  progressBar <- utils::txtProgressBar(
    min = 0,
    max = rowData,
    style = 3
  )

  for (i in 1:rowData) {

    # Try to reconnect with redis
    footballstats::redis_con()

    # Track progress
    utils::setTxtProgressBar(progressBar, i)

    # Take a single slice of match data at a time
    matchSlice <- matchData[i, ]
    matchID <- matchSlice$id %>% as.integer
    KEYS$COMP <- matchSlice$comp_id
    KEYS$TIL <- KEYS$COMP %>% footballstats::teams_in_league()
    teamIDs <- c(matchSlice$localteam_id, matchSlice$visitorteam_id)

    # Get single match information
    singleMatchInfo <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchID) %>%
      rredis::redisHGetAll()

    # 0) datSlice contains the match ID from the start
    datSlice <- data.frame(
      matchID = matchID,
      stringsAsFactors = FALSE
    )

    if (logger) print(datSlice)

    if (i == 51) loggs <<- TRUE else loggs <<- FALSE

    if (logger) print(datSlice)

    #formData <- footballstats::project_form(
    #  KEYS = KEYS,
    #  teamIDs = teamIDs,
    #  currentID = matchID
    #)

    # 2) Get form information
    #datSlice %<>% cbind(
    #  formData
    #)

    # 1) Get commentary information (Initialise datSlice)
    datSlice %<>% cbind(
      footballstats::feat_commentaries(
        KEYS = KEYS,
        matchID = matchID,
        teamIDs = teamIDs,
        commentaryNames = c('shots_total', 'shots_ongoal', 'possesiontime')
      )
    )

    formData <- footballstats::project_form(
      KEYS = KEYS,
      teamIDs = teamIDs,
      currentID = matchID
    )

    # 2) Get form information
    datSlice %<>% cbind(
      formData
    )


    if (logger) print(datSlice)

    # Finally, find out how convincing the match was for the home team
    conv <- matchSlice$ft_score %>%
      footballstats::prs_ftscore()
    convince <- conv[1] - conv[2]

    # From convince, find out result as W / L / D
    res %<>% c(if (convince > 0) 'W' else if (convince == 0) 'D' else 'L')

    # Get relative position
    datSlice %<>% cbind(
      footballstats::feat_position(
        KEYS = KEYS,
        matchID = matchID,
        teamIDs = teamIDs
      )
    )

    if (logger) print(datSlice)

    mDat %<>% rbind(datSlice)
  }

  # Results are collected as a vector, column bind it onto the full dataframe
  mDat$res <- res

  # Close the progress bar
  close(progressBar)

  mDat %>% return()
}

#' @title Form Feature
#'
#' @description A function that takes a matchData data frame and two
#'  teamID values stored in \code{teamIDs}, with a single match piece
#'  of information and calculates the form of the team in the run up
#'  to the match information passed. The functionality is looped for both
#'  teamIDs.
#'
#' @param matchData A data frame that contains rows of single matches
#'  that have been played between two teams.
#' @param teamIDs A character vector of length two, containing the home team
#'  and away team in that order.
#' @param singleMatchInfo A data frame with one row that contains
#'  important match information between two teams.
#'
#' @return A data frame with two columns, `form.h` and `form.a`.
#'
#' @export


feat_form <- function(matchData, teamIDs, singleMatchInfo) {
  forms <- c()
  for (j in 1:2) {
    # Calculate team form
    formResults <- footballstats::team_form(
      matchData = matchData,
      teamID = teamIDs[j]
    )

    # Find out form relative to current date.
    forms %<>% c(
      footballstats::relative_form(
        matchInfo = singleMatchInfo,
        totalForm = formResults
      )
    )
  }

  # Calculate the difference in forms
  form <- if (forms %>% length %>% `!=`(2)) {
    list(NA, NA)
  } else {
    forms %>% lapply(footballstats::form_to_int)
  }

  # Return the data frame with form as the only column
  return(
    data.frame(
      `form.h` = form[[1]],
      `form.a` = form[[2]],
      stringsAsFactors = FALSE
    )
  )
}

#' @title Commentary Feature
#'
#' @description A function that takes matchIDs, teamIDs, and commentaryNames
#'  and tries to query the commentaries from redis to recreate the commentary
#'  information. It is looped over twice for each match, one for the home team
#'  and secondly for the away team.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{cmt:_commentary:{comp_id}:{match_id}:{team_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param matchID A character string that represents the current matchID
#'  under investigation.
#' @param teamIDs A character vector of length two, containing the home team
#'  and away team in that order.
#' @param commentaryNames A character vector of allowed commentaryNames to
#'  be investigated.
#'
#' @return A data frame of commentary data for the home and away team.
#'
#' @export


feat_commentaries <- function(KEYS, matchID, teamIDs, commentaryNames) {

  cResults <- c()
  for (j in 1:2) {
    totOppData <- c()
    allCommentaryKeys <- paste0('cmt_commentary:', KEYS$COMP, ':*:', teamIDs[j]) %>%
      rredis::redisKeys() %>% as.character

    # Check commentary key exists
    if (allCommentaryKeys %>% length %>% `==`(0)) break

    # Get the last 4
    commentaryKeys <- KEYS %>% footballstats::order_commentaries(
      commentaryKeys = allCommentaryKeys
    ) %>% rev

    # Get all the matchIDs
    matchIDs <- commentaryKeys %>%
      footballstats::flatt(y = 3) %>%
      as.integer

    bigger <- matchID %>% `>`(matchIDs)
    if (bigger %>% sum %>% `>`(KEYS$DAYS)) commentaryKeys %<>% `[`(bigger %>% which %>% `[`(1:KEYS$DAYS)) else next

    # Check that all the allowed names is a subset of the commentary
    for (k in 1:(commentaryKeys %>% length)) {
      availableNames <- commentaryKeys[k] %>% rredis::redisHGetAll() %>% names
      if (commentaryNames %in% availableNames %>% all %>% `!`()) break
    }

    # Calculate all the statistics
    allStats <- data.frame(stringsAsFactors = FALSE)
    for (k in 1:(commentaryKeys %>% length)) {
      res <- footballstats::commentary_from_redis(
        keyName = commentaryKeys[k],
        returnItems = commentaryNames
      )
      allStats %<>% rbind(res %>% as.data.frame %>% t)
    }
    names(allStats) <- commentaryNames

    # Calculate opposition strength here!
    newMatchIDs <- commentaryKeys %>%
      footballstats::flatt(y = 3) %>%
      as.integer

    # Get other teams shots on goal
    oShots <- c()
    currentTeam <- teamIDs[j]
    for (k in 1:(allCommentaryKeys %>% length)) {
      if (oShots %>% length %>% `==`(KEYS$DAYS)) next
      bothKeys <- rredis::redisKeys(pattern = paste0('cmt_commentary:', KEYS$COMP, ':', newMatchIDs[k], '*'))
      bothTeams <- bothKeys %>% footballstats::flatt(y = 4)
      otherKey <- if (currentTeam == bothTeams[1]) bothKeys[2] else bothKeys[1]
      otherShots <- otherKey %>% rredis::redisHGet(field = 'shots_ongoal') %>% as.character

      if ("NULL" %in% otherShots) next else otherShots %<>% as.integer

      oShots %<>% c(otherShots)
    }
    oShots %<>% sum

    calcPos <- data.frame()
    concede <- goals <- 0
    for (k in 1:(allCommentaryKeys %>% length)) {
      if (totOppData %>% length %>% `==`(KEYS$DAYS)) next
      oppResults <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', newMatchIDs[k]) %>%
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
        matchID = NULL,
        teamIDs = c(oth, teamIDs[j]),
        matchDate = oppResults[1]
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

    #currentForm <- formData[[j]]

    # Calculate mean and other metrics here
    singleMean <- apply(allStats, 2, mean)
    clinical <- goals %>% `/`(allStats$shots_ongoal %>% sum) %>% `*`(100)
    concede <- concede %>% `/`(oShots) %>% `*`(100)

    totOppData %<>% `/`(KEYS$TIL) %>% `*`(100)

    cResults %<>% c(
      c(singleMean, totOppData %>% mean, clinical, concede, scoreVenue %>% sum, concedeVenue %>% sum) %>%
        as.double %>% list
    )
  }

  allNames <- c(
    commentaryNames %>% paste0('.h'),
    c('strength.h', 'clinical.h', 'defensive.h', 'scored.h', 'concede.h'),
    commentaryNames %>% paste0('.a'),
    c('strength.a', 'clinical.a','defensive.a', 'scored.a', 'concede.a')
  )

  # Return a mini frame containing form information
  allNames %>%
    footballstats::handle_projections(resList = cResults) %>%
    return()
}

#' @title League Position
#'
#' @description A function that takes a matchID, and the teams associated
#'  to calculate their position in the league table whenever that match
#'  was played. If there is no \code{matchDate} supplied then, a query
#'  to redis to figure out the date of the match is carried out. The weekly
#'  positions is queried to figure out the positions for that particular week.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[KEY]} :: \code{c_startDate:{comp_id}:{season}}}
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param matchID A character string that represents the matchID under
#'  investigation.
#' @param teamIDs A character vector of length two that contains the
#'  two teams involved in the match in order of home and away.
#' @param matchDate If NULL the date is queried by in the basic commentary
#'  information, else a date of the form dd.mm.yyyy can be supplied.
#'
#' @return A data frame with two columns, `position.h` and `position.a`.
#'
#' @export


feat_position <- function(KEYS, matchID, teamIDs, matchDate = NULL) {

  # Get the start date
  startDate <- paste0('c_startDate:', KEYS$COMP, ':', KEYS$SEASON) %>%
    KEYS$RED$GET() %>%
    as.integer

  # Get the current date
  currentDate <- if (matchDate %>% is.null) {
    paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchID) %>%
      KEYS$RED$HGET(field = 'formatted_date') %>%
      as.Date(format = '%d.%m.%Y') %>%
      as.integer
  } else {
    matchDate %>%
      as.Date(format = '%d.%m.%Y') %>%
      as.integer
  }

  # Convert to week number
  weekNum <- currentDate %>%
    `-`(startDate) %>%
    `/`(7) %>%
    floor %>%
    `+`(1)

  # Position key
  posKey <- paste0('cw_pl:', KEYS$COMP, ':', KEYS$SEASON, ':')

  # Get the last known position of the two teams
  weekKeys <- posKey %>%
    paste0('*') %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr() %>%
    footballstats::get_weeks()

  # Get the positions from the week being investigated
  positions <- posKey %>%
    paste0(weekKeys %>% `[`(weekNum %>% `-`(weekKeys) %>% abs %>% which.min)) %>%
    KEYS$RED$HGETALL() %>%
    footballstats::create_hash() %>%
    lapply(as.integer)

  # For play offs, positions may not exist (This will be a rough guide!)
  posH <- positions[[teamIDs[1]]]
  posA <- positions[[teamIDs[2]]]

  # If teams have been added as play offs then max them out to teams in league
  if (posH %>% is.null) posH <- KEYS$TIL
  if (posA %>% is.null) posA <- KEYS$TIL

  # Determine & Return relative position as a data.frame
  return(
    data.frame(
      `position.h` = posH,
      `position.a` = posA,
      stringsAsFactors = FALSE
    )
  )
}
