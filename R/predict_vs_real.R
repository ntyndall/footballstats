#' @title Predictions vs Actual Results
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @export


predict_vs_real <- function(KEYS, readyToAnalyse, matches) {

  readyToAnalyse <- readyToAnalyse %>%
    strsplit(split = '[:]') %>%
    purrr::map(5) %>%
    purrr::flatten_chr()

  readyToAnalyse <- intersect(matches$id, readyToAnalyse)
  if (!identical(readyToAnalyse, character(0))) {
    readyLen <- readyToAnalyse %>% length
    cat(paste0(Sys.time(), ' | Checking off ', readyLen, ' already predicted matches. \n'))

    for (i in 1:readyLen) {
      matchID <- readyToAnalyse[i]
      resultKey <- paste0('csdm_pred:', KEYS$COMP, ':', KEYS$SEASON, ':*:', matchID)
      resultKey %<>% rredis::redisKeys()
      predicted <- resultKey %>% rredis::redisHGetAll()
      pre <- predicted$prediction %>% as.character

      # Make sure the prediction is currently empty!
      if (pre %>% `==`('-')) {

        # Convert data to something meaningful
        home <- predicted$home %>% as.character
        away <- predicted$away %>% as.character

        actual <- matches[which(matches$id == matchID), ]
        hm <- actual$localteam_score %>% as.integer
        aw <- actual$visitorteam_score %>% as.integer

        cond <- function(h, a) {
         return(
           ifelse(
             test = home == h && away == a,
             yes = TRUE,
             no = FALSE
            )
          )
        }

        # Was the prediction correct?
        correct <- if (hm > aw) {
          cond(h = 'W', a = 'L')
        } else if (hm == aw) {
          cond(h = 'D', a = 'D')
        } else {
          cond(h = 'L', a = 'W')
        }

        # Update hash with the results of the prediction
        rredis::redisHSet(
          key = resultKey,
          field = 'prediction',
          value =  ifelse(test = correct, yes = 'T', no = 'F') %>% charToRaw()
        )
      }
    }
  }
}

#' @title Create Monthly Report
#'
#' @details This function is triggered via a CRON job,
#'  and prints the results from a months worth of data
#'  to the slack channel #reports
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @export


monthly_report <- function(KEYS, month, year) {
  # Initialise totalTxt
  totalTxt <- c()

  # Get all predictions
  allPredictions <- paste0('csdm*:', year, ':', month, ':*') %>% rredis::redisKeys()

  if (allPredictions %>% is.null %>% `!`()) {

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
    uniqComps <- sapply(1:(competitionIDs %>% length), function(i) {
      allPredictions[paste0(':', competitionIDs[i], ':') %>% grepl(x = allPredictions)]
    })

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
      result <- lapply(1:(singleComp %>% length), function(j) {
        vals <- singleComp[j] %>% rredis::redisHGetAll()
        return(vals$prediction %>% as.character)
      }) %>% purrr::flatten_chr()

      # Subset for complete results only!
      result %<>% subset(result %>% `!=`('-'))

      # Add to vector for slack
      totalTxt %<>% c(
        if (result %>% identical(character(0))) {
          paste0(cInfo, ' None')
        } else {
         per <- result %>%
           `==`('T') %>%
           sum %>%
           `/`(result %>% length) %>%
           scales::percent()
         paste0(cInfo, per)
        }
      )
    }

    # If True send to slack, if not just print out!
    if (KEYS$SLACK_PRNT) {
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

    } else {
      cat(totalTxt %>% paste(collapse = '\n'))
    }
  }
}
