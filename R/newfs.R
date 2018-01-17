#' @title ord keys
#'
#' @export


ord_keys <-function(commentaryKeys, competitionID, seasonStarting) {

  # Get the dates just incase and make sure they are in order:
  matchIDs <- commentaryKeys %>%
    strsplit(split = ':') %>%
    purrr::map(3) %>%
    purrr::flatten_chr()

  csmIDs <- paste0('csm:', competitionID, ':', seasonStarting, ':', matchIDs)

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
  formResults <- team_form(
    matchData = matchData,
    teamID = teamID)

  # Create a data frame of forms and dates.
  totalForm <- data.frame(
    date = formResults[[2]],
    form = formResults[[1]],
    stringsAsFactors = FALSE)
  nr <- totalForm %>% nrow

  df[['form']] <- totalForm$form[c((nr - 2):nr)] %>% rev %>% paste(collapse = '') %>% footballstats::form_to_int()
  df %>% return()
}

#' @title get av
#'
#' @export


get_av <- function(orderedKeys, commentaryNames) {
  bFrame <- data.frame(stringsAsFactors = FALSE)
  ln <- length(orderedKeys)
  for (i in 1:ln) {
    rredis::redisHGetAll(key = orderedKeys[i])
    results <- footballstats::commentary_from_redis(
      keyName = orderedKeys[i],
      returnItems = commentaryNames)

    results %<>% as.data.frame %>% t
    colnames(results) <- commentaryNames
    bFrame %<>% rbind(results)

  }
  bFrame %>% return()
}
