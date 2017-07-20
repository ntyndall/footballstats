#' @title Get Standings
#'
#' @description A function that generates a league table for a given 
#'  competitionID.
#'  
#' @details The end point `/standings/` is accessed to get a dataframe
#'  of the league table.
#'  
#' @import httr
#' 
#' @param competitionID An integer containing the competition ID that the 
#'  teams and match information belong to.
#' @param apiKey An alphanumeric value that contains a given API_KEY
#'  which is loaded into the global environment to allow access to the endpoint.
#' @param host An alphanumeric value that contains a given HOST
#'  which is loaded into the global environment to allow access to the endpoint.
#'  
#' @return A data frame containing the league table status_code == 200
#' @return Null for an appropriate response if status_code != 200
#'


getStandings <- function(competitionID, host = HOST, apiKey = API_KEY) {
  listOfStandings <- httr::GET(paste0(HOST, "/standings/", competitionID, "?", API_KEY))
  if (listOfStandings$status_code == 200) {
    standings <- rawToChar(listOfStandings$content)
    return(jsonlite::fromJSON(standings))
  } else {
    print(paste0(Sys.time(), " : Warning - status code ", listOfStandings$status_code, " in getStandings(). ",
                 "when looking for competition -> ", competitionID))
    return(NULL)
  }
}