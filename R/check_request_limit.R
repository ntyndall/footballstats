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
#' @import rredis
#' 
#' @param requestsAllowed An integer value that defines the number of requests
#'  that can be made in a given time period.
#' @param timePeriod An integer value in seconds that defines the time period
#'  where `requestsAllowed` API calls are allowed.
#'  
#' @return Nothing. Redis is updated with the correct requestLimit values.
#'


check_request_limit <- function(requestsAllowed = 1000, timePeriod = 60 * 60) {
  
  requestCount <- as.integer(redisConnection$INCR(key = "requestLimit"))
  if (requestCount == 1) {
    redisConnection$EXPIRE(key = "requestLimit", seconds = timePeriod - 1 )
  } else {
    if (requestCount > requestsAllowed - 100) {
      print(paste0(Sys.time(), ' : WARNING - requests getting low. Sleeping for one hour.'))
      redisConnection$SET(key = 'requestLimit', value = 0)
      Sys.sleep(60 * 60)
    }
  }
}
