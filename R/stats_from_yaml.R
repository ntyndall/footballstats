#' @title Stats from YAML
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


stats_from_yaml <- function(KEYS) {

  # Data named values
  dValues <- c(
    "shots_total", "shots_ongoal", "possesiontime",
    "fouls", "yellowcards", "redcards", "corners"
  )

  # Find the data from the package
  all.data <- system.file("extdata", "statistics.yaml", package = "footballstats") %>%
    yaml::yaml.load_file()

  # Season names
  seasons <- all.data %>%
    names

   for (s in seasons) {
    # Subset the season
    season.data <- all.data %>%
      `[[`(s)

    # data names
    dNames <- season.data %>%
      names

    # Loop over and check each entry
    for (i in 1:(season.data %>% length)) {
      # Make sure each entry is of length two
      datLens <- season.data[[i]] %>%
        purrr::map(length) %>%
        as.integer

      # If some items are missing, let me know
      if (7 %>% `==`(datLens) %>% all %>% `!`()) {
        cat(' ## Check entry for ID :', dNames[i], '\n')
        next
      }

      # Get basic stat key
      basicKeyName <- "csm:*:" %>%
        paste0(dNames[i]) %>%
        KEYS$RED$KEYS()

      # If basic stats are missing then we have a serious problem!
      if (basicKeyName %>% length %>% `==`(0)) {
        cat(' ## Check CSM entry for ID :', dNames[i],'\n')
        next
      } else {
        basicKeyName %<>% purrr::flatten_chr()
      }

      # Split it up to get the new key names
      statsKey <- basicKeyName %>%
        strsplit(split = ':') %>%
        purrr::flatten_chr()

      # From the stats key, does the commentary exist???
      comKey <- paste0("csmt_commentary:", statsKey[2], ":", s, ":", dNames[i], ":")

      # If 2 don't exist, then populate redis
      if (comKey %>% paste0('*') %>% KEYS$RED$KEYS() %>% length %>% `!=`(2)) {
        # Get the teamIDs
        teamIDs <- statsKey %>%
          paste(collapse = ':') %>%
          KEYS$RED$HMGET(field = c("home.id", "away.id")) %>%
          lapply(as.integer) %>%
          purrr::flatten_int()

        # Create the redis keys
        comKey %<>% paste0(teamIDs)

        # Insert both teamID information
        for (j in 1:2) {
          input <- season.data[[i]][[j]] %>% as.character
          input[3] %<>% paste0('%')
          comKey[j] %>% KEYS$RED$HMSET(
            field = dValues,
            value = input
          )
        }
      }
    }
  }

  # Now check to see which ones haven't been added
  KEYS %>% footballstats::missing_commentaries()
}
