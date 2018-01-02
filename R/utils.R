#' @title sensitive_keys
#'
#' @description A function that loads all sensitive information into a
#'  global namespace for use throughout the code.
#'
#' @details Set up:
#'  1) Fill in the appropriate sensitive information.
#'  2) Copy this file to sensitiveInfo.R and rename the
#'     function to sensitiveInfo()
#'
#' @param None
#'
#' @return Returns nothing.
#'
#' @export


sensitive_keys <- function() {  # nocov start
  print(paste0(Sys.time(), ' : Loading global environment variables...'))
  fsHost <- Sys.getenv("FS_HOST")
  fsApikey <- Sys.getenv("FS_APIKEY")
  fsSlack <- Sys.getenv("FS_SLACK")
  prof <- footballstats::possible_env()

  if (any(nchar(c(fsHost, fsApikey, fsSlack)) < 1)) {
    stop(paste0('Halting - please set environment variables for `FS_HOST`, `FS_APIKEY`, and `FS_SLACK`.',
                '\n Possible locations include :: \n ',
                paste(' -->', prof, collapse = '\n '),
                '\n\n Current values are : \n',
                paste0('FS_HOST = ', fsHost, '\n'),
                paste0('FS_APIKEY = ', fsApikey, '\n'),
                paste0('FS_SLACK = ', fsSlack, '\n')))
  } else {
    return(list(FS_HOST = fsHost,
                FS_APIKEY= fsApikey,
                FS_SLACK = fsSlack))
  }
}  # nocov end

#'
#' @export


possible_env <- function() {  # nocov start
  return(Filter(function(f) nchar(f) > 0, c(
    Sys.getenv("R_PROFILE"),
    file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site"),
    Sys.getenv("R_PROFILE_USER"),
    file.path(getwd(), ".Rprofile"))))

} # nocov end

#'
#' @export


time_intervals <- function() { # nocov start
  lastMatchTime <- redis$GET(key = 'match:lastInterval')
  if (is.null(lastMatchTime)) {
    lastMatchTime <- Sys.Date() - (365 * 2)
    # Adjust to make this fall on a Friday.
    while (weekdays(lastMatchTime) != 'Friday') {
      lastMatchTime <- lastMatchTime + 1
    }
    redis$SET(key = 'match:lastInterval',
              value = as.integer(lastMatchTime))
  } else {
    as.Date(lastMatchTime, origin = "1970-01-01")
  }
  return(lastMatchTime)
} # nocov end

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

  requestCount <- as.integer(rredis::redisIncr(
    key = "requestLimit"))
  if (requestCount == 1) {
    rredis::redisExpire(
      key = "requestLimit",
      seconds = timePeriod - 1 )
  } else {
    if (requestCount > requestsAllowed - 100) {
      cat(paste0(Sys.time(), ' : WARNING - requests getting low. Sleeping for ', timePeriod, ' seconds. \n'))
      rredis::redisSet(
        key = 'requestLimit',
        value = "0" %>% charToRaw())
      Sys.sleep(timePeriod)
    }
  }
}

#' @title Redis Connection
#'
#' @export


redis_con <- function() { # nocov start
  rredis::redisConnect(
    host = 'localhost',
    port = 6379,
    nodelay = FALSE)
  rredis::redisSelect(1)
} # nocov end

