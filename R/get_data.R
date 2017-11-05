#' @title get_data
#'
#' @description A function that generates JSON data from a particular
#'  endpoint provided to the function as a string.
#'  
#' @import httr
#' 
#' @param endpoint A character string that defines the endpoint and parameters
#'  utilized to gather the data. The endpoint is accessed by concatenating the 
#'  string HOST -> endpoint -> apiKey.
#' @param apiKey An alphanumeric value that contains a given API_KEY
#'  which is loaded into the global environment to allow access to the endpoint.
#' @param host An alphanumeric value that contains a given HOST
#'  which is loaded into the global environment to allow access to the endpoint.
#'  
#' @return A list of data corresponding to the `endpoint` if status_code == 200
#' @return Null for an appropriate response if status_code != 200
#'


get_data <- function(endpoint, host = HOST, apiKey = API_KEY) {
  listOfSeasons <- httr::GET(paste0(HOST, endpoint, API_KEY))
  return(listOfSeasons$status_code %>% 
    purrr::when(. == 200 ~ jsonlite::fromJSON(rawToChar(listOfSeasons$content)), ~ NULL))
}