#' @title Result Of Match
#'
#' @description A function that returns a single character value of
#'  'W' / 'L' / 'D' depending on the scores and which team scored them.
#'  
#' @param scoreHome An integer value denoting the home team score.
#' @param scoreAway An integer value denoting the away team score.
#' @param homeOrAway A character vector which is either 'localteam_id'
#'  or 'visitorteam_id', to conduct the correct operation on the two
#'  score parameters.
#'
#' @return Returns one of 'W' / 'L' / 'D'.
#'

resultOfMatch <- function(scoreCurrent, scoreOther) {
  return(c(scoreCurrent, scoreOther) %>% purrr::when(.[1] == .[2] ~ 'D', 
                                                     .[1] > .[2] ~ 'W', 
                                                     ~ 'L'))
}
