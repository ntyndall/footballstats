#' @title Get Dates
#'
#' @description A function that checks whether or not the season
#'  is currently active or not.
#'
#' @export


dates_from_yaml <- function(KEYS, tNow = Sys.Date()) {

  # Define the redis key holding the season
  sKey <- "c_currentSeason:" %>% paste0(KEYS$COMP)

  # Try to get the current season value
  cSeason <- sKey %>%
    KEYS$RED$GET()

  # If it doesn't exist then use date now!
  if (cSeason %>% is.null) {
    cSeason <- tNow %>% format("%Y")
    sKey %>% KEYS$RED$SET(value = cSeason)
  }

  # Load in YAML file of starting and end dates
  myDates <- system.file("extdata", "seasonDates.yaml", package = "footballstats") %>%
    yaml::yaml.load_file()

  # get right data (season and competition) (and does it exist?)
  myDates %<>% `[[`(cSeason) %>% `[[`(KEYS$COMP %>% as.character)

  # Turn off activity until checks are passed
  KEYS$ACTIVE <- FALSE

  # Make sure that the information exists
  if (myDates %>% is.null) {

    # Try updating the season again, otherwise it could be stuck forever
    cSeason <- tNow %>% format("%Y")
    sKey %>% KEYS$RED$SET(value = cSeason)

    # Log details
    cat(
      paste0(
        Sys.time(), " | Supply season dates for season : ", cSeason, " for competition : ", KEYS$COMP, "\n"
      )
    )
  } else {
    # Convert to date time
    newDates <- myDates %>% as.Date(format = "%d.%m.%Y")

    # Make sure the time is now active (inbetween start and end dates)
    if (tNow %>% `>`(newDates[1]) %>% `&&`(tNow %>% `<`(newDates[2]))) {
      KEYS$ACTIVE <- TRUE
      KEYS$DATE_FROM <- myDates[1]
      KEYS$DATE_TO <- tNow %>% `-`(1) %>% footballstats::format_dates()
    } else {
      # If the season has finally ended then update the redis key
      if (tNow %>% `>`(newDates[2])) {
        cSeason <- tNow %>% format("%Y")
        sKey %>% KEYS$RED$SET(
          value = cSeason
        )
      }
    }
  }

  # Update current season
  KEYS$SEASON <- cSeason %>% as.integer

  # Return updated KEYS back
  return(KEYS)
}
