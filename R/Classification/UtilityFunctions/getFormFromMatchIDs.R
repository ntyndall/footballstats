

getFormFromMatchIDs <- function(matchIDs, seasonStarting, matchFieldNames, formLimit = 3) {
  formList <- lapply(1:length(matchIDs), function(k) {
    redisKey <- paste0('csm:', competitionID, ':', seasonStarting, ':', matchIDs[k])
    singleMatch <- redisConnection$HMGET(key = redisKey, 
                                         field = matchFieldNames)
    names(singleMatch) <- matchFieldNames 
    
    # Determine the result of the match for the home team
    singleResult <- resultOfMatch(scoreHome = as.integer(singleMatch$localteam_score), 
                                  scoreAway = as.integer(singleMatch$visitorteam_score), 
                                  homeOrAway = names(which(singleMatch == home)))
    list(singleMatch$formatted_date, singleResult)
  })
  
  totalForm <- data.frame(date = sapply(1:length(formList), function(k) formList[[k]][[1]] ),
                          form = sapply(1:length(formList), function(k) formList[[k]][[2]] ),
                          stringsAsFactors = FALSE)
  
  totalForm <- totalForm[sort.int(totalForm$date, decreasing = FALSE, index.return = TRUE)$ix, ]
  return(paste(as.character(na.omit(totalForm$form[1:formLimit])), collapse = ''))
}
