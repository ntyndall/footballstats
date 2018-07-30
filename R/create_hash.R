#' @title Create Hash
#'
#' @export


create_hash <- function(x) {
  xVec <- x %>% purrr::flatten_chr()
  xList <- xVec %>%
    `[`(c(FALSE, TRUE)) %>%
    as.list

  names(xList) <- xVec %>%
    `[`(c(TRUE, FALSE))

  return(xList)
}
