#' @title Get Teams
#'
#' @description A function that generates match information.
#'  
#' @details The end point `/team/` is accessed to get a list
#'  of all team information from a teamID. (ID's are pre determined
#'  from a matchID query).
#'  
#' @import httr
#' 
#' @param apiKey An alphanumeric value that contains a given API_KEY
#'  which is loaded into the global environment to allow access to the endpoint.
#' @param host An alphanumeric value that contains the particular HOST
#'  of the API which is loaded into the global environment to allow 
#'  access to the endpoint.
#' @param teamID An integer value denoting a particular teamID.
#'  
#' @return A list of team information for a given teamID if status_code == 200
#' @return Null for an appropriate response if status_code != 200
#'


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
