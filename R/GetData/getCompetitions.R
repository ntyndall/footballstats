#' @title Get Competitions
#'
#' @description A function that generates all competition IDs
#'  
#' @details The end point `/competitions/` is accessed to get a list
#'  of all competition IDs accessible and returns them as a list.
#'  
#' @import httr
#' 
#' @param API_KEY An alphanumeric value that contains a given API_KEY
#'  which is loaded into the global environment to allow access to the endpoint.
#'  
#' @return A list of competition IDs and their names if status_code == 200
#' @return Null for an appropriate response if status_code != 200


getCompetitions <- function(host = HOST, apiKey = API_KEY) {
  listOfSeasons <- httr::GET(paste0(HOST, "/competitions?", API_KEY))
  if (listOfSeasons$status_code == 200) {
    seasonIDs <- rawToChar(listOfSeasons$content)
    return(jsonlite::fromJSON(seasonIDs))
  } else {
    print(paste0(Sys.time(), " : Warning - status code ", listOfSeasons$status_code, " in getCompetition()."))
    return(NULL)
  }
}
