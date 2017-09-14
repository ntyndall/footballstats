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


calculateTeamForm <- function(matchData, teamID, whichTeam) {
  
  teamsResult <- dateOfMatch <- c()
  lapply(1:length(whichTeam), function(i) {
    truth <- matchData[[whichTeam[i]]] == teamID
    if (any(truth)) {
      single <- matchData[which(truth), ]
      results <- sapply(1:nrow(single), function(x) {
        resultOfMatch(scoreHome =  as.integer(single$localteam_score[x]), 
                      scoreAway = as.integer(single$visitorteam_score[x]), 
                      homeOrAway = whichTeam[i])
      })
      list(results, as.integer(single$formatted_date))
    } else {
      list('', '')
    }
  })
}
