#' @title Parse Season
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
#' @export


prs_comp <- function(mComps) {
  return(
    mComps %>% unique
  )
}

#' @title Parse Full Time score
#'
#' @param ftScore A character string of the form
#'  '[2-0]', and will be parsed as an integer vector of
#'  2 0.
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
#' @export


flatt <- function(keyVector, y) {
  return(
    keyVector %>%
      strsplit(split = ':') %>%
      purrr::map(y) %>%
      purrr::flatten_chr()
  )
}
