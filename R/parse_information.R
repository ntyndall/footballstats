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


