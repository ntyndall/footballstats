#' @title Map API ID To My ID
#'
#' @description A function that maps the original API ID to my newly
#'  created ID that is generated when a new ID is created
#'  
#' @param APIID An integer ID that is the original ID from web API
#' @param myID An integer ID that has been automatically generated
#'  after a unique 'event' has been identified
#' @param type A string which identifies which attritibute, such as
#'  one of competition/team/match etc
#'  
#' @return returns nothing, redis is updated with the key/value pair
#'


mapAPIToMyID <- function(APIID, myID, type) {
  rredis::redisSet(key = paste0(type, ":mapto:", APIID), value = charToRaw(myID))
}