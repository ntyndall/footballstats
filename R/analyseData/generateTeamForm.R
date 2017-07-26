#' @title Generate Team Form
#' 
#' @description A function that provided with a competitionID generates recent team
#'  form.
#'  
#' @details Given a match data frame, all the matches are analysed and teams 
#'  are provided with their form up to a certain number of matches in the past.
#'  
#' @param competitionID An integer containing the competitionID that the 
#'  teams and match information belong to.
#' @param redisData An environment that defines the redis configuration where data is
#'  to be searched for.  
#'  
#' @return Nothing. Redis is updated with relevant form information.
#' 


generateTeamForm <- function(competitionID, redisData) {
  
}