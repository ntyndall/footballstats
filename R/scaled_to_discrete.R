#' @title Scaled To Discrete
#'
#' @export


scaled_to_discrete <- function(data.set, boundLen) {

  # Calculate the boundaries between 0 and 1
  bounds <- seq(
    from = 0,
    to = 1 + 1e-7,
    length.out = boundLen + 1
  )

  # Loop over
  for (i in 1:(data.set %>% ncol %>% `-`(1))) {
    data.set[[i]] %<>% findInterval(bounds)
  }

  return(data.set)
}
