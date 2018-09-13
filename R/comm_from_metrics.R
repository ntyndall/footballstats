#' @title Comm From Metrics
#'
#' @export


comm_from_metrics <- function(my.metrics, comp = "1204", season = "2017") {

  # Append % onto possession
  for (i in paste0("possesiontime", c(".a", ".h"))) my.metrics[[i]] %<>% paste0("%")

  # Convert to character list
  my.metrics %<>% lapply(as.character)

  # Set up the Redis Keys
  rKeys <- paste0(
    "csmt_commentary:", comp, ":", season, ":", my.metrics$matchID %>% rep(each = 2),
    ":", my.metrics$localID %>% rbind(my.metrics$awayID) %>% as.vector
  )

  # Get all the names
  allNames <- my.metrics %>% names

  # Loop over both home and away
  for (i in 1:2) {
    if (i == 1) {
      # Sort out home
      type <- ".h"
      keysub <- c(TRUE, FALSE)
    } else {
      type <- ".a"
      keysub <- c(FALSE, TRUE)
    }

    newNames <- allNames %>% grepl(pattern = type, fixed = TRUE)
    newKeys <- rKeys %>% `[`(keysub)
    stat.data <- my.metrics %>% `[`(newNames) %>% utils::head(-1)
    metricNames <- allNames %>% `[`(newNames) %>% utils::head(-1) %>% strsplit(split = "[.]") %>% purrr::map(1) %>% as.character

    KEYS$RED$pipeline(
      .commands = lapply(
        X = 1:(newKeys %>% length),
        FUN = function(x) {
          newKeys[x] %>% KEYS$PIPE$HMSET(
            field = metricNames,
            value = stat.data %>% purrr::map(x) %>% purrr::flatten_chr()
          )
        }
      )
    )
  }
}
