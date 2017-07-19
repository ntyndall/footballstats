#' @title Get Matches
#'
#' @description A function that generates match information.
#'  
#' @details The end point `/matches/` is accessed to get a list
#'  of all match IDs accessible and returns them as a list.
#'  
#' @import httr
#' 
#' @param apiKey An alphanumeric value that contains a given API_KEY
#'  which is loaded into the global environment to allow access to the endpoint.
#' @param host An alphanumeric value that contains the particular HOST
#'  of the API which is loaded into the global environment to allow 
#'  access to the endpoint.
#' @param competitionID An integer value of length 4 denoting the competition
#'  ID noted by the API.
#' @param dateFrom A POSIXct value converted to dd.mm.yyyy format which denotes
#'  the start date for querying the API.
#' @param dateTo A POSIXct value converted to dd.mm.yyyy format which denotes
#'  the end date for querying the API.
#'  
#' @return A list of competition IDs and their names if status_code == 200
#' @return Null for an appropriate response if status_code != 200


getTeams <- function(teamID, host = HOST, apiKey = API_KEY) {
  teamInfo <- httr::GET(paste0(HOST, "/team/", teamID, "?", API_KEY))
  if (teamInfo$status_code == 200) {
    team <- rawToChar(teamInfo$content)
    return(jsonlite::fromJSON(team))
  } else {
    print(paste0(Sys.time(), " : Warning - status code ", teamInfo$status_code, " in getTeams(). ",
                 "when looking for team -> ", teamID))
    return(NULL)
  }
}
