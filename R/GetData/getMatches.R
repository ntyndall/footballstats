#' @title Get Matches
#'
#' @description A function that generates match information.
#'  
#' @details The end point `/matches/` is accessed to get a list
#'  of all matchIDs.
#'  
#' @import httr
#' 
#' @param competitionID An integer value of length 4 denoting the competition
#'  ID noted by the API.
#' @param dateFrom A POSIXct value converted to dd.mm.yyyy format which denotes
#'  the start date for querying the API.
#' @param dateTo A POSIXct value converted to dd.mm.yyyy format which denotes
#'  the end date for querying the API.
#' @param apiKey An alphanumeric value that contains a given API_KEY
#'  which is loaded into the global environment to allow access to the endpoint.
#' @param host An alphanumeric value that contains the particular HOST
#'  of the API which is loaded into the global environment to allow 
#'  access to the endpoint.
#'  
#' @return A list of matchIDs and relevant information if status_code == 200
#' @return Null for an appropriate response if status_code != 200
#'


getMatches <- function(competitionID, dateFrom, dateTo,
                       host = HOST, apiKey = API_KEY) {
  listOfMatches <- httr::GET(paste0(HOST, "/matches", 
                                    "?comp_id=", competitionID,
                                    "&from_date=", dateFrom,
                                    "&to_date=", dateTo,
                                    "&", API_KEY))
  if (listOfMatches$status_code == 200) {
    matches <- rawToChar(listOfMatches$content)
    return(jsonlite::fromJSON(matches))
  } else {
    return(NULL)
  }
}
