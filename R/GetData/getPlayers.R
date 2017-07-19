

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
