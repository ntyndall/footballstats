#' @title Order Commentaries
#'
#' @description A function that takes a vector of commentary key names
#'  that are stored in redis, the date is checked to see when they happened
#'  and then that vector is ordered and sorted and returned back in the
#'  correct order.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param commentaryKeys A character vector of commentary key names as
#'  found in redis.
#'
#' @return A character vector of the same commentaryKeys but ordered
#'  by date in which they occured.
#'
#' @export


order_commentaries <-function(KEYS, commentaryKeys) {

  # Get the dates just incase and make sure they are in order:
  matchIDs <- commentaryKeys %>%
    footballstats::flatt(y = 4)

  # Get formatted dates from redis as a vector
  dates <- KEYS$RED$pipeline(
    .commands = lapply(
      X = paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchIDs),
      FUN = function(x) x %>% KEYS$PIPE$HGET("zzz.date")
    )
  ) %>%
    purrr::flatten_chr() %>%
    as.Date('%d.%m.%Y') %>%
    as.integer %>%
    order

  # Order keys by the formatted date
  return(commentaryKeys %>% `[`(dates))
}
