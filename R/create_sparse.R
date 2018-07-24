#' @title Create Sparse
#'
#' @description A function used for creating a sparse matrix
#'  with \code{(ncol - 1)} features and \code{1} column of
#'  results, i.e. W / L / D.
#'
#' @export


create_sparse <- function(my.data, boundLen) {
  myDimNames <- indI <- indJ <- c()
  allNames <- my.data %>%
    colnames %>%
    `[`(1:(my.data %>% ncol %>% `-`(1)))
  startLen <- 0

  for (k in 1:(my.data %>% ncol %>% `-`(1))) {
    for (j in 1:(my.data %>% nrow)) {
      indJ %<>% c(startLen + my.data[j, k])
      indI %<>% c(j)
    }
    myDimNames %<>% c(allNames[k] %>% paste0('=', c(1:boundLen)))
    startLen %<>% `+`(boundLen)
  }

  # Return the sparse matrix
  # How do I ensure there are trailing zeros?
  return(
    Matrix::sparseMatrix(
      i = indI,
      j = indJ,
      x = 1,
      dimnames = list(NULL, myDimNames)
    )
  )
}
