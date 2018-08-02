#' @title Create Hash
#'
#' @export


create_hash <- function(x) {

  # Check if any are raw
  anyRaw <- x %>%
    lapply(typeof) %>%
    purrr::map(function(z) z %>% `==`("raw")) %>%
    purrr::flatten_lgl()

  # If raw then replace with character
  if (anyRaw %>% any) x[anyRaw] %<>% lapply(redux::bin_to_object) %>% lapply(as.character)

  # Now start to flatten and rename
  xVec <- x %>% purrr::flatten_chr()
  xList <- xVec %>%
    `[`(c(FALSE, TRUE)) %>%
    as.list

  names(xList) <- xVec %>%
    `[`(c(TRUE, FALSE))

  return(xList)
}
