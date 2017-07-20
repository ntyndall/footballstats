#' @title Get Players
#'
#' @description A function that generates player information given a 
#'  computed playerID from elsewhere (e.g. team or match information)
#'  
#' @details The end point `/players/` is accessed to get a list
#'  of all player statistics.
#'  
#' @import httr
#' 
#' @param playerID An integer value that corresponds to a particular playerID.
#' @param apiKey An alphanumeric value that contains a given API_KEY
#'  which is loaded into the global environment to allow access to the endpoint.
#' @param host An alphanumeric value that contains a given HOST
#'  which is loaded into the global environment to allow access to the endpoint.
#'  
#' @return A list of player statistics if status_code == 200
#' @return Null for an appropriate response if status_code != 200
#'


getPlayers <- function(playerID, host = HOST, apiKey = API_KEY) {
  playerInfo <- httr::GET(paste0(HOST, "/player/", playerID, "?", API_KEY))
  if (playerInfo$status_code == 200) {
    player <- rawToChar(playerInfo$content)
    return(jsonlite::fromJSON(player))
  } else {
    print(paste0(Sys.time(), " : Warning - status code ", playerInfo$status_code, " in getPlayers(). ",
                 "when looking for player -> ", playerID))
    return(NULL)
  }
}
