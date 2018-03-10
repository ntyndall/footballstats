#' @title Order Commentaries
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
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
#' @param teamID ...
#' @param matchData ...
#'
#' @return An integer value
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
      tail(3) %>%
      rev %>%
      paste(collapse = '') %>%
      footballstats::form_to_int()
  )
}

#' @title Commentary Frame
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
