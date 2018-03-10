#' @title Parse Season
#'
#' @description A function that takes a season form
#'  and returns the correct season starting value.
#'
#' @param mSeasons A character vector of the form
#'  2017/2018. (Can also take multiple, not sure of the
#'  benefit just yet)
#'
#' @return A character string of yyyy format.
#'
#' @export


prs_season <- function(mSeasons) {
  return(
    mSeasons %>%
      unique %>%
      strsplit(split = '[/]') %>%
      purrr::map(1) %>%
      purrr::flatten_chr()
  )
}

#' @title Parse Competition
#'
#' @description A function that parses competitionIDs
#'  and returns only the unique IDs.
#'
#' @param mComps A character vector of competitionIDs.
#'
#' @return A character vector of unique competitionIDs.
#'
#' @export


prs_comp <- function(mComps) {
  return(
    mComps %>% unique
  )
}

#' @title Parse Full Time score
#'
#' @description A function that takes a full time result and
#'  tries to parse this as an integer vector of H vs. A.
#'
#' @param ftScore A character string of the form
#'  '[2-0]', and will be parsed as an integer vector of
#'  2 0.
#'
#' @return An integer vector of the form c(home_score, away_score).
#'
#' @export


prs_ftscore <- function(ftScore) {
  return(
  ftScore %>%
    strsplit(split = '[[:punct:]]') %>%
    purrr::flatten_chr() %>%
    `[`(c(2:3)) %>%
    as.integer
  )
}

#' @title Split, Map, and Flatten
#'
#' @description A function that is used in multiple locations
#'  to help split the redis key format at a particular value
#'  around the ':'.
#'
#' @param keyVector A character vector of redis keys of the form
#'  a:b:c.
#' @param y An integer value to define where to split and flatten the
#'  keys, i.e. if y = 2 split on a: _ : b
#'
#' @return A character vector of the values on the split.
#'
#' @export


flatt <- function(keyVector, y) {
  return(
    keyVector %>%
      strsplit(split = ':') %>%
      purrr::map(y) %>%
      purrr::flatten_chr()
  )
}
