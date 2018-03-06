#' @title ord keys
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @export


ord_keys <-function(commentaryKeys, KEYS) {

  # Get the dates just incase and make sure they are in order:
  matchIDs <- commentaryKeys %>%
    footballstats::flatt(y = 3)

  csmIDs <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchIDs)

  dates <- c()
  for (k in 1:(csmIDs %>% length)) {
    dates %<>% c(
      csmIDs[k] %>%
        rredis::redisHGet(field = 'formatted_date') %>%
        as.character
    )
  }

  # Order keys by the formatted date
  ordKeys <- commentaryKeys[
    dates %>%
      as.Date('%d.%m.%Y') %>%
      as.integer %>%
      order
    ]

  ordKeys %>% return()
}

#' @title get frm
#'
#' @export


get_frm <- function(df, teamID, matchData) {
  formResults <- footballstats::team_form(
    matchData = matchData,
    teamID = teamID
  )

  # Create a data frame of forms and dates.
  totalForm <- data.frame(
    date = formResults[[2]],
    form = formResults[[1]],
    stringsAsFactors = FALSE
  )
  nr <- totalForm %>% nrow

  df[['form']] <- totalForm$form[c((nr - 2):nr)] %>%
    rev %>%
    paste(collapse = '') %>%
    footballstats::form_to_int()
  df %>% return()
}

#' @title get av
#'
#' @export


get_av <- function(orderedKeys, commentaryNames) {
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
