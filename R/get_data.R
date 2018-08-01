#' @title Get Data
#'
#' @description A function that generates JSON data from a particular
#'  endpoint provided to the function as a string.
#'
#' @param endpoint A character string that defines the endpoint and parameters
#'  utilized to gather the data. The endpoint is accessed by concatenating the
#'  string HOST -> endpoint -> apiKey.
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @return A list of data corresponding to the `endpoint` if status_code == 200,
#'  or NULL for an appropriate response if status_code != 200
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
    return(list(status_code = 600))
  })

  # Increment the request limit
  KEYS %>% footballstats::request_limit()

  # Record the bad request
  if (rawContent$status_code != 200) print(paste0(' ## WARNING : Could not resolve ', getUrl))

  return(
    rawContent$status_code %>% purrr::when(
      . == 200 ~ rawContent$content %>%
        rawToChar() %>%
        jsonlite::fromJSON(),
      ~ NULL
    )
  )
} # nocov end
