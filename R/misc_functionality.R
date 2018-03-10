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
    footballstats::flatt(y = 3)

  csmIDs <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchIDs)

  # Get formatted dates as a vector
  dates <- csmIDs %>%
    lapply(function(x) x %>% rredis::redisHGet(field = 'formatted_date')) %>%
    lapply(as.character) %>%
    purrr::flatten_chr()

  # Order keys by the formatted date
  return(
    commentaryKeys %>%
      `[`(dates %>% as.Date('%d.%m.%Y') %>% as.integer %>% order)
  )
}

#' @title Form From MatchData
#'
#' @description A function that takes a matchData data.frame
#'  and a teamID and calculates the form cast as an integer
#'  value.
#'
#' @param teamID A character string that defines the current
#'  teamID that is under investigation.
#' @param matchData A data frame that contains rows of single matches
#'  that have been played between two teams.
#'
#' @return An integer value representing the form.
#'
#' @export


form_from_matchdata <- function(teamID, matchData) {
  # Calculate a list of form and dates
  formResults <- footballstats::team_form(
    matchData = matchData,
    teamID = teamID
  )

  # Return the LAST 3 form and calculate integer score
  return(
    formResults$form %>%
      utils::tail(3) %>%
      rev %>%
      paste(collapse = '') %>%
      footballstats::form_to_int()
  )
}

#' @title Commentary Frame
#'
#' @description A function that takes ordered keys and
#'  the names from the commentary and takes them from redis
#'  and creates a data frame with number of rows equal to
#'  the length of the keys, provided they all exist and have
#'  non null values.
#'
#' @param orderedKeys A character vector of commentary key names
#'  that are stored in redis in some order.
#' @param commentaryNames A character vector defining the field
#'  names of the key names to be returned back to store in the
#'  new data frame.
#'
#' @return A data frame containing all the commentary information
#'  with the number of columns equal to that of the \code{commentaryNames}.
#'
#' @export


commentary_frame <- function(orderedKeys, commentaryNames) {
  bFrame <- data.frame(stringsAsFactors = FALSE)
  ln <- length(orderedKeys)
  for (i in 1:ln) {
    results <- footballstats::commentary_from_redis(
      keyName = orderedKeys[i],
      returnItems = commentaryNames
    )

    if (results %>% is.null) next

    results %<>% as.data.frame %>% t
    colnames(results) <- commentaryNames
    bFrame %<>% rbind(results)

  }
  bFrame %>% return()
}
