

classify_form_from_match <- function(competitionID, matchIDs, seasonStarting, matchFieldNames, teamID, formLimit = 3) {
  formList <- lapply(1:length(matchIDs), function(k) {
    redisKey <- paste0('csm:', competitionID, ':', seasonStarting, ':', matchIDs[k])
    singleMatch <- redisConnection$HMGET(key = redisKey, 
                                         field = matchFieldNames)
    names(singleMatch) <- matchFieldNames 
    
    # Need to choose which current team is being analysed for each match.
    currentTeam <- matchFieldNames[as.integer(which(singleMatch == teamID))]
    
    scoreCurrent <- as.integer(singleMatch[currentTeam %>% purrr::when(. == 'localteam_id' ~ 'localteam_score', ~ 'visitorteam_score')])
    scoreOther <-  as.integer(singleMatch[currentTeam %>% purrr::when(. == 'localteam_id' ~ 'visitorteam_score', ~ 'localteam_score')])
    
    # Determine the result of the match for the current team
    singleResult <- classify_match_result(scoreCurrent = scoreCurrent, 
                                          scoreOther = scoreOther)
    list(singleMatch$formatted_date, singleResult)
  })
  
  totalForm <- data.frame(date = sapply(1:length(formList), function(k) formList[[k]][[1]] ),
                          form = sapply(1:length(formList), function(k) formList[[k]][[2]] ),
                          stringsAsFactors = FALSE)
  
  totalForm <- totalForm[sort.int(totalForm$date, decreasing = FALSE, index.return = TRUE)$ix, ]
  return(paste(as.character(na.omit(totalForm$form[1:formLimit])), collapse = ''))
}
