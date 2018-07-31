#' @title Get Last Week
#'
#' @export


get_last_week <- function(KEYS, uniqueTeams) {
  prevKeys <- paste0('cwt_l:', KEYS$COMP, ':', KEYS$SEASON, ':*') %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr()

  toMatch <- prevKeys %>%
    strsplit(split = ':') %>%
    purrr::map(5) %>%
    purrr::flatten_chr()

  myWeeks <- prevKeys %>%
    footballstats::get_weeks()

  maxWeeks <- lapply(
    X = uniqueTeams,
    FUN = function(y) {
      matchedUp <- y %>% `==`(toMatch)
      if (matchedUp %>% any) {
        myWeeks %>% `[`(matchedUp) %>% max
      } else {
        NULL
      }
    }
  )


  missingWeeks <- maxWeeks %>%
    purrr::map(is.null) %>%
    purrr::flatten_lgl()

  if (missingWeeks %>% any) {
    zerodMatches <- uniqueTeams %>% `[`(missingWeeks)
    zeros <- lapply(zerodMatches, FUN = function(x) list(PTS = "0", GF = "0", GD = "0"))
    names(zeros) <- zerodMatches
    zeroWeeks <- 0 %>% rep(zerodMatches %>% length)

    uniqueTeams %<>% `[`(missingWeeks %>% `!`())
    maxWeeks %>% purrr::compact()
  } else {
    zeros <- list()
    zeroWeeks <- c()
  }

  # If keys do exist then return their values
  if (maxWeeks %>% length %>% `>`(0)) {
    maxWeeks %<>% purrr::flatten_chr()
    rKeys <- paste0('cwt_l:', KEYS$COMP, ':', KEYS$SEASON, ":", maxWeeks, ":", uniqueTeams)
    allResults <- KEYS$RED$pipeline(
      .commands = lapply(
        X = rKeys,
        FUN = function(x) x %>% KEYS$PIPE$HGETALL()
      )
    ) %>% lapply(footballstats::create_hash)
    names(allResults) <- uniqueTeams
  } else {
    allResults <- c()
  }

  # Return information back
  return(
    list(
      table = allResults %<>% c(zeros),
      weeks = maxWeeks %<>% c(zeroWeeks)
    )
  )
}
