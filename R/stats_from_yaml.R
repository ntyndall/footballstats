#' @title
#'
#' @description A function to insert statistics not returned
#'  by the API from a well-defined yaml file.
#'
#' @details This function takes an input list as defined in
#'  \code{missing_data/statistics.yaml} and reads the home and away
#'  statistics, makes sure they exist in redis, and if they dont
#'  then it reads the values and populates by inferring the competition
#'  etc.
#'
#' @export


stats_from_yaml <- function() {

  # Data named values
  dValues <- c(
    "shots_total", "shots_ongoal", "possesiontime",
    "fouls", "yellowcards", "redcards", "corners"
  )

  # Connect to Redis
  footballstats::redis_con()

  # Load data file
  newData <- getwd() %>%
    paste0('/missing_data/statistics.yaml') %>%
    yaml::yaml.load_file()

  # data names
  dNames <- newData %>% names

  # Loop over and check each entry
  for (i in 1:(newData %>% length)) {
    statsKey <- "csm:*:" %>%
      paste0(dNames[i]) %>%
      rredis::redisKeys() %>%
      strsplit(split = ':') %>%
      purrr::flatten_chr()

    # From the stats key, does the commentary exist???
    comKey <- paste0("cmt_commentary:", statsKey[2], ":", dNames[i], ":")

    # If 2 don't exist, then populate redis
    if (comKey %>% paste0('*') %>% rredis::redisKeys() %>% length %>% `!=`(2)) {
      # Get the teamIDs
      teamIDs <- statsKey %>%
        paste(collapse = ':') %>%
        rredis::redisHMGet(field = c('localteam_id', 'visitorteam_id')) %>%
        lapply(as.integer) %>%
        purrr::flatten_int()

      # Create the redis keys
      comKey %<>% paste0(teamIDs)

      # Insert both teamID information
      for (j in 1:2) {
        input <- newData[[i]][[j]] %>% as.list
        names(input) <- dValues
        input$possesiontime %<>% paste0('%')
        comKey[j] %>% rredis::redisHMSet(
          values = input
        )
      }
    }
  }
}

