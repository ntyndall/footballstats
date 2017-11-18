#' @title sensitive_keys
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

sensitive_keys <- function(fsHost = "", fsApiKey = "", fsSlack = "") {
  return(list(FS_HOST = fsHost,
              FS_APIKEY= fsApikey,
              FS_SLACK = fsSlack))
}
