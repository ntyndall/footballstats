#' @title Other Score
#'
#' @export


other_score <- function(oth) {
  return(
    if (oth %>% `==`('W')) 'L' else if (oth %>% `==`('L')) 'W' else 'D'
  )
}
