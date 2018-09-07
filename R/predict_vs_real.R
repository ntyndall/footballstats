#' @title Predictions vs Actual Results
#'
#' @description A function that takes the prediction keys and the gathered
#'  matchData and decides whether the match has been played and then decides
#'  whether the match was a 'W' / 'L' / 'D' pivoted on the home team,
#'  and checks the prediction if it was correct (T) or incorrect (F).
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{csdm_pred:{comp_id}:{season}:{month}:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param readyToAnalyse A character vector of the form of the redis keys.
#' @param matches A data frame that contains rows of single matches
#'  that have been played between two teams.
#'
#' @return Nothing. The prediction key for a particular match is updated with
#'  a T or F whether the prediction was true or false.
#'
#' @export


predict_vs_real <- function(KEYS, readyToAnalyse, matches) {

  # Get the matchIDs where keys exist
  readyToAnalyse %<>%
    footballstats::flatt(5)

  # All predicted matchIDs
  allPreds <- "all_predictions" %>%
    KEYS$RED$SMEMBERS() %>%
    purrr::flatten_chr()

  # Which matches have been played yet?
  matched <- allPreds %in% matches$id

  # Initialise matchIDs
  matchIDs <- c()

  # If any are to be appended, check now
  if (matched %>% any) {
    # Subset
    allPreds %<>% `[`(matched)

    # Do the keys exist
    matched2 <- allPreds %in% readyToAnalyse

    # Also make sure it has a key
    if (matched2 %>% any) {
      # Subset and assign matchIDs here
      matchIDs <- allPreds %>% `[`(matched2)

      # Remove them from the set
      KEYS$RED$pipeline(
        .commands = lapply(
          X = matchIDs,
          FUN = function(x) "all_predictions" %>% KEYS$PIPE$SREM(x)
        )
      )
    }
  }

  # Make sure some are to be analysed
  if (matchIDs %>% length %>% `>`(0)) {
    readyLen <- matchIDs %>% length
    if (KEYS$LOGGING) cat(paste0(Sys.time(), ' | Checking off ', readyLen, ' already predicted matches. \n'))

    # MatchIDs
    resultKeys <- paste0('csdm_pred:', KEYS$COMP, ':', KEYS$SEASON, ':*:', matchIDs)

    # Get all the keys to be updated
    redKeys <- KEYS$RED$pipeline(
      .commands = lapply(
        X = resultKeys,
        FUN = function(x) x %>% KEYS$PIPE$KEYS()
      )
    ) %>%
      purrr::map(1) %>%
      purrr::flatten_chr()

    # Now get all current redis information
    currentData <- KEYS$RED$pipeline(
      .commands = lapply(
        X = redKeys,
        FUN = function(x) x %>% KEYS$PIPE$HGETALL()
      )
    ) %>%
      lapply(footballstats::create_hash)

    # Which values should be predicted
    toPredict <- sapply(
      X = 1:(currentData %>% length),
      FUN = function(x) currentData[[x]]$prediction %>% `==`("-")
    )

    # Filter out if predictions are to be made
    if (toPredict %>% any) {
      currentData %<>% `[`(toPredict)
      matchIDs %<>% `[`(toPredict)
      redKeys %<>% `[`(toPredict)

      # Subset the full matches data set and convert to list
      matchList <- matches %>%
        subset(matches$id %in% matchIDs) %>%
        `[`(c("id", "localteam_score", "visitorteam_score")) %>%
        lapply(as.integer)

      # Set up function for calculating results
      get_res <- function(x) if (x > 0) "W" else if (x < 0) "L" else "D"

      # Get the result for all matches
      matchList$homeres <- sapply(
        X = matchList$localteam_score %>% `-`(matchList$visitorteam_score),
        FUN = function(x) x %>% get_res()
      )

      # Update redis keys whether the prediction was successful or not
      KEYS$RED$pipeline(
        .commands = lapply(
          X = 1:(currentData %>% length),
          FUN = function(x) {
            redKeys[x] %>% KEYS$PIPE$HSET(
              field = "prediction",
              value = if (currentData[[x]]$home %>% `==`(matchList$homeres[x])) "T" else "F"
            )
          }
        )
      )
    }
  }
}

#' @title Create Monthly Report
#'
#' @description This function is triggered via a CRON job,
#'  and prints the results from a months worth of data
#'  to the slack channel #reports
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param month An integer value defining the month that the match was
#'  played in.
#' @param year An integer value defining the season that the match was
#'  played in.
#'
#' @return Nothing. The monthly report is either printed to screen or
#'  sent directly to Slack.
#'
#' @export


monthly_report <- function(KEYS, month, year) {
  # Initialise totalTxt
  totalTxt <- c()

  # Get all predictions
  allPredictions <- paste0('csdm*:', year, ':', month, ':*') %>%
    KEYS$RED$KEYS()

  if (allPredictions %>% length %>% `>`(0)) {
    allPredictions %<>% purrr::flatten_chr()

    totalTxt %<>% paste(c('*Looking at predictions for', month.abb[month], '*'), collapse = '')
    # Get all unique competitionIDs
    competitionIDs <- allPredictions %>%
      strsplit(split = '[:]') %>%
      purrr::map(2) %>%
      purrr::flatten_chr() %>%
      unique

    # Load competition info
    compInfo <- footballstats::compInfo

    # Create a list of each competition
    uniqComps <- lapply(
      X = competitionIDs,
      FUN = function(i) allPredictions[paste0(':', i, ':') %>% grepl(x = allPredictions)]
    ) %>%
      purrr::flatten_chr()

    # Loop over unique competitions
    for (i in 1:(uniqComps %>% length)) {
      # Set the list as singleComp
      singleComp <- uniqComps[[i]]

      # Get name and region
      info <- competitionIDs[i] %>%
        `==`(compInfo$id) %>%
        which

      # Save header details
      cInfo <- paste0('_', compInfo$region[info], ' - ', compInfo$name[info], '_ :: ')

      # Get the results
      result <- KEYS$RED$pipeline(
        .commands = lapply(
          X = singleComp,
          FUN = function(j) j %>% KEYS$PIPE$HGET("prediction")
        )
      ) %>%
        purrr::flatten_chr()

      # Subset for complete results only!
      result %<>% subset(result %>% `!=`(''))

      # Add to vector for slack
      totalTxt %<>% c(
        paste0(
          cInfo,
          if (result %>% length %>% `==`(0)) {
            " None"
          } else {
            result %>%
              `==`('T') %>%
              sum %>%
              `/`(result %>% length) %>%
              scales::percent()
          }
        )
      )
    }

    # If True send to slack, if not just print out!
    if (KEYS$SLACK_PRNT) { # nocov start
      slackr::slackrSetup(
        channel = '#general',
        api_token = KEYS$FS_SLACK
      )

      slackr::slackr_msg(
        txt = totalTxt,
        channel = '#general',
        api_token = KEYS$FS_SLACK,
        username = 'report'
      )
    } else { # nocov end
      if (KEYS$LOGGING) cat(totalTxt %>% paste(collapse = '\n'))
    }
  }
}
