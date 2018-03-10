#' @title From Vector To Integer
#'
#' @description A function that converts a form vector e.g. 'WLD' into an integer
#'  value defined by the function default values for win / lose / draw.
#'
#' @param oldForms A character string which contains either `W`, `L`, or `D`
#'  of arbitrary length
#' @param winPoints An integer defining the points accredited for a win.
#' @param drawPoints Same as above for a draw.
#' @param losePoints Same as above for a loss.
#'
#' @examples
#'  footballstats::form_to_int('WLDW')
#'
#' @return An integer value defining the value of a teams form
#'
#' @export


form_to_int <- function(oldForms, winPoints = 2, drawPoints = 1, losePoints = 0) {
  # Create a character vector of W / L / D
  oldForms %<>% strsplit(split = '') %>% purrr::flatten_chr()

  # Set up function to sum each of W / L / D
  sumPts <- function(x) oldForms %>% `==`(x) %>% sum %>% return()

  # Sum up all the available points
  return(
    'W' %>% sumPts() %>% `*`(winPoints) %>%
      `+`('D' %>% sumPts() %>% `*`(drawPoints)) %>%
      `+`('L' %>% sumPts() %>% `*`(losePoints))
  )
}

#' @title Relative Form
#'
#' @description A function that calculates the relative form from an
#'  arbitrary date, derived from the current date found in matchInfo and
#'  the data frame of total results and the dates which they happened.
#'
#' @param matchInfo A list / data frame object that is a slice of data
#'  of the form of matchData, which contains useful match information and
#'  the date.
#' @totalForm A named list object which contains the \code{$date} and \code{$form}
#'  values, each vector is of equal length.
#'
#' @return An integer value of the current relative form as an integer.
#'
#' @export


relative_form <- function(matchInfo, totalForm) {
  # Determine current match date
  currentDate <- matchInfo$formatted_date %>%
    as.Date('%d.%m.%Y') %>%
    as.integer

  totalForm %<>% as.data.frame
  # Determine total data frame of forms ordered highest to lowest

  totalForm <- totalForm[
    sort.int(
      x = totalForm$date,
      decreasing = TRUE,
      index.return = TRUE
    )$ix,
  ]

  # Calculate the relative form
  formsHaveNotOccured <- sum(currentDate <= totalForm$date)
  relativeForm <- totalForm[-c(1:formsHaveNotOccured), ]

  formsInOrder <- relativeForm$form
  if (length(formsInOrder) >= 3) {
    return(paste(formsInOrder[1:3], collapse = ''))
  } else {
    return(NULL)
  }
}


#' @title Team Form
#'
#' @description A function that generates both the team form and date of
#'  a particular team.
#'
#' @param matchData A data frame that contains rows of single matches
#'  that have been played between two teams.
#' @param teamID A character string that denotes the current team being
#'  analysed.
#'
#' @return Returns a named list with `date` and `form`.
#'
#' @export


team_form <- function(matchData, teamID) {
  teamID %<>% as.integer
  teamsResult <- dateOfMatch <- c()
  singleTeam <- matchData[matchData$localteam_id == teamID | matchData$visitorteam_id == teamID, ]

  # Go through each match the team has played
  results <- sapply(1:nrow(singleTeam), function(x) {
    # Need to choose which current team is being analysed for each match.
    scorers <- footballstats::current_or_other(
      singleMatch = singleTeam[x, ] %>% as.list,
      teamID = teamID
    )

    # Determine the result of the match for the current team
    footballstats::match_result(
      scoreCurrent = scorers$current,
      scoreOther = scorers$other
    )
  })

  # Return a list of results
  return(
    list(
      date = singleTeam$formatted_date %>%
        as.Date('%d.%m.%Y') %>%
        as.integer,
      form = results
    )
  )
}

#' @title Current Or Other
#'
#' @description Take a single slice of match data and a teamID
#'  and work out whether the ID belongs to the home team or the away
#'  team and return a list of the current teamIDs score and the other
#'  teamIDs score (don't need to know this ID).
#'
#' @param singleMatch A list / data frame object that is a single slice
#'  of a matchData data frame that contains information such as
#'  localteam_id, localteam_score (plus visitorteam information and
#'  scorelines etc).
#' @param teamID An integer value that represents the team ID which
#'  we want to determine the scoreline from.
#'
#' @return A named list object with \code{home} and \code{away} values,
#'  which is inferred from the teamID.
#'
#' @examples
#'  \dontrun{
#'   singleMatch <- list(localteam_id = '1000', localteam_score = '2')
#'   result <- singleMatch %>% footballstats::current_or_other(teamID = 1000)
#'   # result == list(current = 2, other = ...)
#'  }
#'
#' @export


current_or_other <- function(singleMatch, teamID) {

  # Assume localteam first, if not, then reverse the vector
  order <- c('localteam_score', 'visitorteam_score')
  if (singleMatch$visitorteam_id %>% as.integer %>% `==`(teamID)) order %<>% rev

  # Return the score id's of the match slice as  integers
  homeaway <- singleMatch[order] %>% as.integer
  return(list(current = homeaway[1], other = homeaway[2]))
}

#' @title Match Result
#'
#' @description A function that returns a single character value of
#'  'W' / 'L' / 'D' depending on the scores and which team scored them.
#'
#' @param scoreCurrent An integer value denoting the home team score.
#' @param scoreOther An integer value denoting the away team score.
#'
#' @return Returns one of 'W' / 'L' / 'D'.
#'
#' @export


match_result <- function(scoreCurrent, scoreOther) {
  return(
    c(scoreCurrent, scoreOther) %>%
      purrr::when(.[1] == .[2] ~ 'D', .[1] > .[2] ~ 'W', 'L')
   )
}
