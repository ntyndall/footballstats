#' @title utils_mock_sensitive
#'
#' @description A function that loads all sensitive information into a 
#'  global namespace for use throughout the code.
#'  
#' @details Set up: 
#'  1) Fill in the appropriate sensitive information.
#'  2) Copy this file to sensitiveInfo.R and rename the 
#'     function to sensitiveInfo()
#'
#' @param None
#'  
#' @return Returns nothing.
#'

utils_mock_sensitive <- function() {
  HOST <<- ""
  API_KEY <<- ""
  ROOT <<- ""
  DIR_LOCATION <<- paste0(ROOT, "/R")
  SLACK <<- ""
}
