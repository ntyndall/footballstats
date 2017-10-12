#' @title Calculate Team Form
#'
#' @description A function that generates both the team form and date of 
#'  the match in a list. A list is generated for both of these in terms 
#'  of a home and away list.
#'  
#' @param matchData A dataframe containing all the match data, so it can
#'  be subsetting easily during the vectorized function.
#' @param teamID A character string that denotes the current team being
#'  analysed.
#' @param whichTeam A vector to denote either home / away team used in
#'  both subsetting and generating result of the matches.
#'
#' @return Returns two lists nested inside one list.
#'


calculateTeamForm <- function(matchData, teamID) {
  teamID <- as.integer(teamID)
  teamsResult <- dateOfMatch <- c()
  singleTeam <- matchData[matchData$localteam_id == teamID | matchData$visitorteam_id == teamID, ]

  # Go through each match the team has played
  results <- sapply(1:nrow(singleTeam), function(x) {
    singleMatch <- as.list(singleTeam[x, ])
    
    currentTeam <- singleMatch[as.integer(which(singleMatch == as.character(teamID)))]
    scoreCurrent <- as.integer(singleMatch[currentTeam %>% purrr::when(. == 'localteam_id' ~ 'localteam_score', ~ 'visitorteam_score')])
    scoreOther <-  as.integer(singleMatch[currentTeam %>% purrr::when(. == 'localteam_id' ~ 'visitorteam_score', ~ 'localteam_score')])
    
    resultOfMatch(scoreCurrent =  scoreCurrent, 
                  scoreOther = scoreOther)
  })
  return(list(results, as.integer(singleTeam$formatted_date)))
}
