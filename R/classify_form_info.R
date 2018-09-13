#' @title From Vector To Integer
#'
#' @description A function that converts a form vector e.g. 'WLD' into an integer
#'  value defined by the function default values for win / lose / draw.
#'
#' @param oldForms A character string which contains either `W`, `L`, or `D`
#'  of arbitrary length
#' @param winPoints An integer defining the points accredited for a win.
#' @param drawPoints Same as above for a draw.
#' @param losePoints Same as above for a loss.
#'
#' @examples
#'  footballstats::form_to_int('WLDW')
#'
#' @return An integer value defining the value of a teams form
#'
#' @export


form_to_int <- function(oldForms, winPoints = 2, drawPoints = 1, losePoints = 0) {
  # Create a character vector of W / L / D
  oldForms %<>% strsplit(split = '') %>% purrr::flatten_chr()

  # Set up function to sum each of W / L / D
  sumPts <- function(x) oldForms %>% `==`(x) %>% sum %>% return()

  # Sum up all the available points
  return(
    'W' %>% sumPts() %>% `*`(winPoints) %>%
      `+`('D' %>% sumPts() %>% `*`(drawPoints)) %>%
      `+`('L' %>% sumPts() %>% `*`(losePoints))
  )
}

#' @title Match Result
#'
#' @description A function that returns a single character value of
#'  'W' / 'L' / 'D' depending on the scores and which team scored them.
#'
#' @param scoreCurrent An integer value denoting the home team score.
#' @param scoreOther An integer value denoting the away team score.
#'
#' @return Returns one of 'W' / 'L' / 'D'.
#'
#' @export


match_result <- function(scoreCurrent, scoreOther) {
  return(
    c(scoreCurrent, scoreOther) %>%
      purrr::when(.[1] == .[2] ~ 'D', .[1] > .[2] ~ 'W', 'L')
   )
}
