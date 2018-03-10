#' @title sensitive_keys
#'
#' @description A function that loads all sensitive information into a
#'  global namespace for use throughout the code.
#'
#' @details Set up:
#'  \itemize{
#'  \item Fill in the  appropriate sensitive information.
#'   \item Copy this file to sensitiveInfo.R and rename the
#'     function to sensitiveInfo
#' }
#'
#' @param printToSlack A boolean
#' @param testing A boolean
#' @param storePred A boolean
#'
#' @return Returns nothing.
#'
#' @export


sensitive_keys <- function(printToSlack, printToScreen, testing, storePred) {  # nocov start
  cat(paste0(Sys.time(), ' | Loading global environment variables...'))
  fsHost <- Sys.getenv("FS_HOST")
  fsApikey <- Sys.getenv("FS_APIKEY")
  fsSlack <- Sys.getenv("FS_SLACK")
  prof <- footballstats::possible_env()

  if (c(fsHost, fsApikey, fsSlack) %>% nchar %>% `<`(1) %>% any) {
    stop(
      paste0(
        'Halting - please set environment variables for `FS_HOST`, `FS_APIKEY`, and `FS_SLACK`.',
        '\n Possible locations include :: \n ',
        paste(' -->', prof, collapse = '\n '),
        '\n\n Current values are : \n',
        paste0('FS_HOST = ', fsHost, '\n'),
        paste0('FS_APIKEY = ', fsApikey, '\n'),
        paste0('FS_SLACK = ', fsSlack, '\n')
      )
    )
  } else {
    list(
      FS_HOST = fsHost,
      FS_APIKEY= fsApikey,
      FS_SLACK = fsSlack,
      SLACK_PRNT = printToSlack,
      TEST = testing,
      LOGGING = printToScreen,
      LOG_PRED = storePred,
      DAYS = 4,
      STAND = 0.5
    ) %>%
    return()
  }
}  # nocov end

#' @title Possible Environment
#' @export


possible_env <- function() {  # nocov start
  return(
    Filter(
      f = function(f) nchar(f) > 0,
      x = c(
        Sys.getenv("R_PROFILE"),
        file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site"),
        Sys.getenv("R_PROFILE_USER"),
        file.path(getwd(), ".Rprofile")
      )
    )
  )
} # nocov end

#' @title API Date Formatter
#'
#' @export


format_dates <- function(standardDateFormat) {
  day <- format(x = standardDateFormat, "%d")
  month <- format(x = standardDateFormat, "%m")
  year <- format(x = standardDateFormat, "%y")
  return(paste0(day, '.', month, '.20', year))
}

#' @title Current Season
#'
#' @export


start_season <- function() {
  frm <- function(f) Sys.Date() %>% format(f) %>% as.integer
  currentSeason <- frm("%Y")
  return(if (`<`(frm("%m"), 7)) currentSeason - 1 else currentSeason)
}

#' @title check_request_limit
#'
#' @description A function that stores the number of requests made to the
#'  API within a given time period. The redis key is available through
#'     ->   requestLimit
#'  If limit is reached then the code will stall until requests are free
#'  to query the API again successfully.
#'
#' @details The API is constrained to 1000 request per hour (default), or
#'  x calls per t time. So this function is checked each time before an
#'  endpoint is hit and waits a given time if no requests are remaining.
#'
#'  Redis Keys used;
#'   \itemize{
#'     \item{\strong{[KEY]} :: \code{requestLimit}}
#'   }
#'
#' @param requestsAllowed An integer value that defines the number of requests
#'  that can be made in a given time period.
#' @param timePeriod An integer value in seconds that defines the time period
#'  where `requestsAllowed` API calls are allowed.
#'
#' @return Nothing. Redis is updated with the correct requestLimit values.
#'
#' @export


request_limit <- function(requestsAllowed = 1000, timePeriod = 60 * 60) {

  requestCount <- "requestLimit" %>% rredis::redisIncr() %>% as.integer
  if (requestCount == 1) {
    rredis::redisExpire(
      key = "requestLimit",
      seconds = timePeriod - 1
    )
  } else {
    if (requestCount > requestsAllowed - 100) {
      cat(paste0(' { requests low. Sleeping for ', timePeriod, ' seconds. } '))
      rredis::redisSet(
        key = 'requestLimit',
        value = "0" %>% charToRaw()
      )
      Sys.sleep(timePeriod)
    }
  }
}

#' @title Redis Connection
#'
#' @export


redis_con <- function() { # nocov start
  tryCatch({
    rredis::redisCmd('PING')
  }, error = function(e) {
    rredis::redisConnect(
      host = 'localhost',
      port = 6379,
      nodelay = FALSE
    )
    blnk <- utils::capture.output(rredis::redisSelect(1))
    cat(paste0(Sys.time(), ' | Redis Connection established. \n'))
  })
} # nocov end

#' @title Allowed competitions
#'
#' @export


allowed_comps <- function() { # nocov start
  c('1204', '1205', '1221', '1229', '1269', '1352', '1425') %>%
    return()
} # nocov end


#' @title Create Sink
#'
#' @export


create_sink <- function(fName) { # nocov start
  fName %<>% paste0('_', Sys.Date() %>% format('%d-%m-%y'), '.log')

  # Create the file
  logFile <- file(
    description = paste0('/root/logs/', fName),
    open = "wt"
  )

  # Create the sink
  logFile %>% sink()
} # nocov end

#' @title Create Log Directory
#'
#' @export


create_log_dir <- function() { # nocov start
  # Check the log path global R environment variable
  cat(' ## Reading log path from global variable ... ')
  logPath <- Sys.getenv('FS_DEPLOYLOC')
  if (logPath %>% `==`('')) {
    cat('error. \n')
    stop(
      'Cannot find `FS_DEPLOYLOC`, \n check : \n\n',
      paste(footballstats::possible_env(), collapse = '\n ')
    )
  }
  cat('complete. \n')

  # Create the log path if it doesn't exist already
  logPath <- paste0(logPath, 'logs/')
  if (!dir.exists(logPath)) {
    cat(paste0(' ## Creating log path @ ', logPath, ' \n'))
    dir.create(
      path = logPath,
      recursive = TRUE,
      showWarnings = FALSE
    )
  } else {
    cat(paste0(' ## Log path exists already @ ', logPath, ' \n'))
  }
} # nocov end
