#' @title Get ID List
#'
#' @description A function that loads the character vector of actions into
#' a global variable for use across the application.
#'  
#' @details A list of actions can be defined here where IDs are generated
#'  and endpoints used according from the API.
#'
#' @param None
#'  
#' @return Returns nothing, a vector is loaded into the global environment.
#'

getIDList <- function() {
  ID_LIST <<- c("team", "match", "singleEvent", "event", "competition")
}