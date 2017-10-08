#' @title Map Form To Integer
#'
#' @description A function that converts a form vector e.g. 'WLD' into an integer
#'  value defined by the function default values for win / lose / draw.
#'  
#' @param oldForms A character string which contains either `W`, `L`, or `D`.
#' @param winPoints An integer defining the points accredited for a win.
#' @param drawPoints Same as above for a draw.
#' @param losePoints Same as above for a loss.
#' 
#' @return An integer value defining the value of a teams form
#'


mapFormToInteger <- function(oldForms, winPoints = 2, drawPoints = 1, losePoints = 0) {
  oldForms <- strsplit(oldForms, '')
  newForms <- sapply(1:length(oldForms), function(x) {
    wld <- oldForms[[x]]
    as.integer((sum(wld == "W")*winPoints) + (sum(wld == "D")*drawPoints) + (sum(wld == "L")*losePoints))
  })
  return(newForms)
}
