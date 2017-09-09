#' @title Get Commentaries
#'
#' @description A function that generates match commentary information.
#'  
#' @details The end point `/commentary/` is accessed to get a list
#'  of all commentary information from a matchID. (ID's are pre determined
#'  from a matchID query).
#'  
#' @import httr
#' 
#' @param apiKey An alphanumeric value that contains a given API_KEY
#'  which is loaded into the global environment to allow access to the endpoint.
#' @param host An alphanumeric value that contains the particular HOST
#'  of the API which is loaded into the global environment to allow 
#'  access to the endpoint.
#' @param matchID An integer value denoting a particular matchID
#'  
#' @return A list of commentary information for a given matchID if status_code == 200
#' @return Null for an appropriate response if status_code != 200
#'


getCommentaryInfo <- function(matchID, host = HOST, apiKey = API_KEY) {
  commentaryInfo <- httr::GET(paste0(HOST, "/commentaries/", matchID, "?", API_KEY))
  if (commentaryInfo$status_code == 200) {
    commentary <- rawToChar(commentaryInfo$content)
    return(jsonlite::fromJSON(commentary))
  } else {
    return(NULL)
  }
}
