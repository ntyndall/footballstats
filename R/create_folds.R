#' @title Create Folds
#'
#'
#' @export


create_folds <- function(vec) {
  # Create fold data information
  return(
      list(
      FOLDS = caret::createFolds(
        y = vec,
        k = 10,
        list = TRUE,
        returnTrain = FALSE
      ),
      NUM = 10,
      PER = 7
    )
  )
}
