#' @title classify_form_to_int
#'
#' @description A function that converts a form vector e.g. 'WLD' into an integer
#'  value defined by the function default values for win / lose / draw.
#'  
#' @param oldForms A character string which contains either `W`, `L`, or `D`.
#' @param winPoints An integer defining the points accredited for a win.
#' @param drawPoints Same as above for a draw.
#' @param losePoints Same as above for a loss.
#' 
#' @return An integer value defining the value of a teams form
#'


form_to_int <- function(oldForms, winPoints = 2, drawPoints = 1, losePoints = 0) {
  oldForms <- strsplit(oldForms, '')
  newForms <- sapply(1:length(oldForms), function(x) {
    wld <- oldForms[[x]]
    as.integer((sum(wld == "W")*winPoints) + (sum(wld == "D")*drawPoints) + (sum(wld == "L")*losePoints))
  })
  return(newForms)
}


form_from_match <- function(competitionID, matchIDs, seasonStarting, matchFieldNames, 
                            teamID, formLimit = 3) {
  formList <- lapply(1:length(matchIDs), function(k) {
    redisKey <- paste0('csm:', competitionID, ':', seasonStarting, ':', matchIDs[k])
    singleMatch <- rredis::redisHMGet(
      key = redisKey, 
      fields = matchFieldNames)
    
    # Need to choose which current team is being analysed for each match.
    currentTeam <- matchFieldNames[as.integer(which(singleMatch == teamID))]
    
    scoreCurrent <- as.integer(singleMatch[currentTeam %>% 
                                             purrr::when(. == 'localteam_id' ~ 'localteam_score', ~ 'visitorteam_score')])
    scoreOther <-  as.integer(singleMatch[currentTeam %>% 
                                            purrr::when(. == 'localteam_id' ~ 'visitorteam_score', ~ 'localteam_score')])
    
    # Determine the result of the match for the current team
    singleResult <- footballstats::match_result(
      scoreCurrent = scoreCurrent, 
      scoreOther = scoreOther)
    list(singleMatch$formatted_date, singleResult)
  })
  
  totalForm <- data.frame(date = sapply(1:length(formList), function(k) formList[[k]][[1]] ),
                          form = sapply(1:length(formList), function(k) formList[[k]][[2]] ),
                          stringsAsFactors = FALSE)
  
  totalForm <- totalForm[sort.int(totalForm$date, decreasing = FALSE, index.return = TRUE)$ix, ]
  return(paste(as.character(na.omit(totalForm$form[1:formLimit])), collapse = ''))
}


relative_form <- function(matchInfo, totalForm) {
  # Determine current match date
  currentDate <- as.integer(as.Date(matchInfo$formatted_date, '%d.%m.%Y'))
  
  # Determine total data frame of forms ordered highest to lowest
  totalForm <- totalForm[sort.int(totalForm$date, decreasing = TRUE, index.return = TRUE)$ix, ]
  
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


#' @title classify_team_form
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


team_form <- function(matchData, teamID) {
  teamID <- as.integer(teamID)
  teamsResult <- dateOfMatch <- c()
  singleTeam <- matchData[matchData$localteam_id == teamID | matchData$visitorteam_id == teamID, ]
  
  # Go through each match the team has played
  results <- sapply(1:nrow(singleTeam), function(x) {
    singleMatch <- as.list(singleTeam[x, ])
    
    currentTeam <- singleMatch[as.integer(which(singleMatch == as.character(teamID)))]
    scoreCurrent <- as.integer(singleMatch[currentTeam %>% 
                                             purrr::when(. == 'localteam_id' ~ 'localteam_score', 
                                                         ~ 'visitorteam_score')])
    scoreOther <-  as.integer(singleMatch[currentTeam %>% 
                                            purrr::when(. == 'localteam_id' ~ 'visitorteam_score', 
                                                        ~ 'localteam_score')])
    
    match_result(scoreCurrent =  scoreCurrent, 
                 scoreOther = scoreOther)
  })
  return(list(results, as.integer(singleTeam$formatted_date)))
}
