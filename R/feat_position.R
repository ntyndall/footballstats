#' @title League Position
#'
#' @description A function that takes a matchID, and the teams associated
#'  to calculate their position in the league table whenever that match
#'  was played. If there is no \code{matchDate} supplied then, a query
#'  to redis to figure out the date of the match is carried out. The weekly
#'  positions is queried to figure out the positions for that particular week.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[KEY]} :: \code{c_startDate:{comp_id}:{season}}}
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param matchID A character string that represents the matchID under
#'  investigation.
#' @param teamIDs A character vector of length two that contains the
#'  two teams involved in the match in order of home and away.
#' @param matchDate If NULL the date is queried by in the basic commentary
#'  information, else a date of the form dd.mm.yyyy can be supplied.
#'
#' @return A data frame with two columns, `position.h` and `position.a`.
#'
#' @export


feat_position <- function(KEYS, matchID, teamIDs, matchDate = NULL, formatter = "%d.%m.%Y") {

  # Get the start date
  startDate <- paste0('c_startDate:', KEYS$COMP, ':', KEYS$SEASON) %>%
    KEYS$RED$GET() %>%
    as.integer

  # Get the current date
  currentDate <- if (matchDate %>% is.null) {
    paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchID) %>%
      KEYS$RED$HGET(field = 'formatted_date') %>%
      as.Date(format = formatter) %>%
      as.integer
  } else {
    matchDate %>%
      as.Date(format = formatter) %>%
      as.integer
  }

  # Convert to week number
  weekNum <- currentDate %>%
    `-`(startDate) %>%
    `/`(7) %>%
    floor %>%
    `+`(1)

  # Position key
  posKey <- paste0('cw_pl:', KEYS$COMP, ':', KEYS$SEASON, ':')

  # Get the last known position of the two teams
  weekKeys <- posKey %>%
    paste0('*') %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr() %>%
    footballstats::get_weeks()

  # Get the positions from the week being investigated
  positions <- posKey %>%
    paste0(weekKeys %>% `[`(weekNum %>% `-`(weekKeys) %>% abs %>% which.min)) %>%
    KEYS$RED$HGETALL() %>%
    footballstats::create_hash() %>%
    lapply(as.integer)

  # For play offs, positions may not exist (This will be a rough guide!)
  posH <- positions[[teamIDs[1]]]
  posA <- positions[[teamIDs[2]]]

  # If teams have been added as play offs then max them out to teams in league
  if (posH %>% is.null) posH <- KEYS$TIL
  if (posA %>% is.null) posA <- KEYS$TIL

  # Determine & Return relative position as a data.frame
  return(
    data.frame(
      `position.h` = posH,
      `position.a` = posA,
      stringsAsFactors = FALSE
    )
  )
}
