#' @title Insert Head To Head
#'
#' @export


insert_headtohead <- function(KEYS) {

  # Set the very first date
  "c_startDate:1204:2017" %>%
    KEYS$RED$SET(
      value = "17389"
    )

  # Rename data set
  dat <- footballstats::headtohead

  # Set up commentary data
  comNames <- dat$commentaryData %>% names

  # Add all commentary data
  KEYS$RED$pipeline(
    .commands = lapply(
      X = 1:(dat$commentaryKeys %>% length),
      FUN = function(x) {
        dat$commentaryKeys[x] %>% KEYS$PIPE$HMSET(
          field = comNames,
          value =  dat$commentaryData %>% purrr::map(x) %>% as.character
        )
      }
    )
  )

  # Set up basic data
  basNames <- dat$matches %>% names
  basicKeys <- paste0("csm:1204:2017:", dat$matches$id)

  # Add all basic data
  KEYS$RED$pipeline(
    .commands = lapply(
      X = 1:(basicKeys %>% length),
      FUN = function(x) {
        basicKeys[x] %>% KEYS$PIPE$HMSET(
          field = basNames,
          value =  dat$matches %>% purrr::map(x) %>% as.character
        )
      }
    )
  )

  # Set up position data
  posNames <- dat$positions %>% names

  # Add all position data
  KEYS$RED$pipeline(
    .commands = lapply(
      X = 1:(posNames %>% length),
      FUN = function(x) {
        single <- dat$positions[[x]]
        posNames[x] %>% KEYS$PIPE$HMSET(
          field = single %>% names,
          value = single %>% as.character
        )
      }
    )
  )
}
