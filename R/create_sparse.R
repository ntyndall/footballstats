#' @title Create Sparse
#'
#' @description A function used for creating a sparse matrix
#'  with \code{(ncol - 1)} features and \code{1} column of
#'  results, i.e. W / L / D.
#'
#' @export


create_sparse <- function(my.data, boundLen) {

  # (n-1) columns
  featCols <- my.data %>%
    ncol %>%
    `-`(1)

  # Get the feature names
  allNames <- my.data %>%
    colnames %>%
    `[`(1:featCols)

  # Initialise the starting position and vectors
  myDimNames <- indI <- indJ <- c()
  startLen <- 0

  for (k in 1:featCols) {
    for (j in 1:(my.data %>% nrow)) {
      indJ %<>% c(startLen + my.data[j, k])
      indI %<>% c(j)
    }
    myDimNames %<>% c(allNames[k] %>% paste0('=', c(1:boundLen)))
    startLen %<>% `+`(boundLen)
  }

  # Create a matrix, with a value of 2 in the right most column, to ensure all
  # features are captured
  m <- Matrix::sparseMatrix(
    i = c(1, indI),
    j = c(featCols %>% `*`(boundLen), indJ),
    x = c(0, 1 %>% rep(indI %>% length)),
    dimnames = list(NULL, myDimNames)
  )

  # Finally, drop the zero in the last column
  m %<>% Matrix::drop0()

  # Return the sparse matrix
  return(m)
}
