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

sensitive_keys <- function() {
  print('Loading global environment variables...')
  fsHost <- Sys.getenv("FS_HOST")
  fsApikey <- Sys.getenv("FS_APIKEY")
  fsSlack <- Sys.getenv("FS_SLACK")
  
  return(list(FS_HOST = fsHost,
              FS_APIKEY= fsApikey,
              FS_SLACK = fsSlack))
}


time_intervals <- function() {
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
}


format_dates <- function(standardDateFormat) {
  day <- format(x = standardDateFormat, "%d")
  month <- format(x = standardDateFormat, "%m")
  year <- format(x = standardDateFormat, "%y")
  return(paste0(day, '.', month, '.20', year))
}
