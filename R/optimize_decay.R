#' @title Optimize Decay
#'
#' @description A function to calculate the weights for a
#'  decaying function.
#'
#' @details This function takes in the number of rows of a
#'  data set being analysed plus a decay factor which is just
#'  an exponent value, then the values of this vector are normalised
#'  to 1 so their weights total 1.
#'
#' @param nrows An integer value defining the number of rows of the
#'  data set being analysed.
#' @param decay A double value that is the exponent value of the decaying
#'  function.
#'
#' @export


optimize_decay <- function(nrows, decay) {

  # Use negative exponential to get decay (severity of decay through `decay`)
  vals <- 1:nrows %>%
    `*`(-decay) %>%
    exp()

  # Normalise values to 1 and return
  return(vals %>% `/`(vals %>% sum))
}

