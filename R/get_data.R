#' @title get_data
#'
#' @description A function that generates JSON data from a particular
#'  endpoint provided to the function as a string.
#'
#' @param endpoint A character string that defines the endpoint and parameters
#'  utilized to gather the data. The endpoint is accessed by concatenating the
#'  string HOST -> endpoint -> apiKey.
#' @param apiKey An alphanumeric value that contains a given API_KEY
#'  which is loaded into the global environment to allow access to the endpoint.
#' @param host An alphanumeric value that contains a given HOST
#'  which is loaded into the global environment to allow access to the endpoint.
#'
#' @return A list of data corresponding to the `endpoint` if status_code == 200
#' @return Null for an appropriate response if status_code != 200
#'
#' @export


get_data <- function(endpoint, KEYS) { # nocov start
  getUrl <- paste0(KEYS$FS_HOST, endpoint, KEYS$FS_APIKEY)
  rawContent <- tryCatch({
    httr::RETRY(
      verb = "GET",
      url = getUrl,
      times = 3,
      httr::config(timeout = 30)
    )
  }, error = function(e) {
    cat(paste0(' ## WARNING : Could not resolve ', getUrl))
    list(status_code = 600)
  })

  return(
    rawContent$status_code %>% purrr::when(
      . == 200 ~ rawContent$content %>%
        rawToChar() %>%
        jsonlite::fromJSON(),
      ~ NULL
    )
  )
} # nocov end
