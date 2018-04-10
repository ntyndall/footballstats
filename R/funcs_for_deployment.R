#' @title Predict Fixtures
#'
#' @description A function to be run as a CRON job during
#'  deployment. It gathers some data from redis, tries to
#'  project features and then builds a data set which is
#'  run against some predefined (or dynamic) model carried
#'  in the package.
#'
#' @param deployed A boolean value to indicate whether the
#'  function is being run from a deployed environment.
#'
#' @return Nothing. Redis is updated.
#'
#' @importFrom magrittr %>% %<>% %T>% %$%
#'
#' @export


predict_fixtures <- function(deployed = FALSE) { # nocov start

  # Obtain API and sensitive key information
  KEYS <<- footballstats::sensitive_keys(
    printToSlack = TRUE,
    printToScreen = FALSE,
    testing = FALSE,
    storePred = TRUE
  )

  # Get dates for querying fixutres now
  KEYS$DATE_FROM <- Sys.Date() %>% `+`(1) %>% footballstats::format_dates()
  KEYS$DATE_TO <- Sys.Date() %>% `+`(7) %>% footballstats::format_dates()
  KEYS$SEASON <- footballstats::start_season()

  # Make a connection to redis for storing data
  footballstats::redis_con()

  # Load competitions and run the functionality below.
  competitions <- KEYS %>% footballstats::acomp_info()

  # Subset the available competitions
  competitions %<>% subset(competitions$id %in% footballstats::allowed_comps())

  # Create the sink for output
  if (deployed) 'summary_pred' %>% footballstats::create_sink()
  cat(' -- Beginning analysis -- \n\n\n')

  # Loop over all competitions being analysed
  totalPredictions <- 0
  for (i in 1:nrow(competitions)) {
    cat(paste0(Sys.time(), ' | Storing ' , i, ' / ', nrow(competitions), ' (',
               competitions$name[i], ' - ', competitions$region[i], '). \n'))

    # Predict actual future results
    KEYS$COMP <- competitions$id[i]
    KEYS$TIL <- KEYS$COMP %>% footballstats::teams_in_league()
    KEYS$COMP_NAME <- competitions$name[i]

    cat(paste0(Sys.time(), ' | Predicting actual upcoming fixtures. \n'))
    predictions <- KEYS %>%
      footballstats::predict_matches()
    totalPredictions %<>% `+`(predictions)

    # Send a slack message to indicate how many matches have been predicted
    if (KEYS$SLACK_PRNT && i == nrow(competitions)) {
      slackr::slackrSetup(
        channel = '#results',
        api_token = KEYS$FS_SLACK
      )
      slackr::slackr_msg(
        txt = paste0('Predicted a total of ', totalPredictions, ' matches'),
        channel = '#results',
        api_token = KEYS$FS_SLACK,
        username = 'predictions'
      )
    }
  }
} # nocov end

#' @title Analyse Players
#'
#' @description A function that analyses players only,
#'  this is to be run as a CRON job in deployment.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[LIST]} :: \code{analysePlayers}}
#'   }
#'
#' @param deployed A boolean value to indicate whether the
#'  function is being run from a deployed environment.
#'
#' @return Nothing. Redis is updated.
#'
#' @export


analyse_players <- function(deployed = FALSE) { # nocov start
  # Obtain API and sensitive key information
  KEYS <- footballstats::sensitive_keys(
    printToSlack = TRUE,
    printToScreen = FALSE,
    testing = FALSE,
    storePred = TRUE
  )

  # Store additional KEY information
  KEYS$SEASON <- footballstats::start_season()

  # Make a connection to redis for storing data
  footballstats::redis_con()

  # Create the sink for output
  if (deployed) 'summary_players' %>% footballstats::create_sink()

  # Add player information
  playerLength <- 'analysePlayers' %>% rredis::redisLLen() %>% as.integer
  if (playerLength > 0) {
    cat(paste(' ## Analysing a total of ', playerLength, ' unique players. \n'))
    tNow <- Sys.time()
    KEYS %>% footballstats::aplayer_info(
      playerLength = playerLength
    )
    cat(' ## Players analysed with a \n')
    cat(paste(' ## ', Sys.time() - tNow))
  }

  # Only complete - delete the analysePlayers key (if it exists..)
  if (rredis::redisExists(key = 'analysePlayers')) 'analysePlayers' %>% rredis::redisDelete()

} # nocov end

#' @title Analyse Match and Event
#'
#' @description A function that analyses matches and events only,
#'  this is to be run as a CRON job in deployment.
#'
#' @param deployed A boolean value to indicate whether the
#'  function is being run from a deployed environment.
#'
#' @return Nothing. Redis is updated.
#' @export


analyse_data <- function(deployed = FALSE) { # nocov start
  # Obtain API and sensitive key information
  KEYS <- footballstats::sensitive_keys(
    printToSlack = TRUE,
    printToScreen = FALSE,
    testing = FALSE,
    storePred = TRUE
  )

  # Set up additional keys required for the main flow
  KEYS$SEASON <- footballstats::start_season()
  KEYS$DATE_FROM <- paste0('31.07.', KEYS$SEASON)
  KEYS$DATE_TO <- (Sys.Date() - 1) %>% footballstats::format_dates()

  # Make a connection to redis for storing data
  footballstats::redis_con()

  # Load competitions and run the functionality below.
  competitions <- KEYS %>% footballstats::acomp_info()

  # Subset the available competitions
  competitions %<>% subset(competitions$id %in% footballstats::allowed_comps())

  # Create the sink for adding data
  if (deployed) 'summary_adding' %>% footballstats::create_sink()

  # Loop over all competitions being analysed
  for (i in 1:nrow(competitions)) {
    cat(
      ' ## Storing ::' , i, '/', nrow(competitions), '(',
      competitions$name[i], '-', competitions$region[i], '). \n'
    )

    KEYS$COMP <- competitions$id[i]
    KEYS$TIL <- KEYS$COMP %>% footballstats::teams_in_league()
    KEYS %>% footballstats::add_all()
  }
} # nocov end

#' @title Send Report
#'
#' @description A function to send a monthly report
#'
#' @return Nothing. Redis is updated.
#'
#' @export


send_report <- function() { # nocov start

  # Re-establish redis if necessary
  footballstats::redis_con()

  # Not sure I can properly get the year...

  # Get the month and year for LAST month (i.e. the report to be created)
  month <- Sys.Date() %>% `-`(30) %>% format('%m') %>% as.integer
  Sys.Date() %>% footballstats::prs_season()
  #year <- Sys.Date() %>% `-`(30) %>% format('%Y') %>% as.integer

} # nocov end
