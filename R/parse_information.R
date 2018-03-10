#' @title Parse Season
#' @export


prs_season <- function(mSeasons) {
  mSeasons %>%
    unique %>%
    strsplit(split = '[/]') %>%
    purrr::map(1) %>%
    purrr::flatten_chr() %>%
    return()
}

#' @title Parse Competition
#' @export


prs_comp <- function(mComps) {
  mComps %>%
    unique %>%
    return()
}

#' @title Parse Full Time score
#'
#' @param ftScore A character string of the form
#'  '[2-0]', and will be parsed as an integer vector of
#'  2 0.
#'
#' @export


prs_ftscore <- function(ftScore) {
  conv <- ftScore %>%
    strsplit(split = '[[:punct:]]') %>%
    purrr::flatten_chr() %>%
    `[`(c(2:3)) %>%
    as.integer %>%
    return()
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
