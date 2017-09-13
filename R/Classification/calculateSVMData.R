

calculateSVMData <- function(competitionID, seasonStarting, commentaryKeys, matchData,
                             returnItems, totalData = data.frame(stringsAsFactors = FALSE)) {
  for (i in 1:length(commentaryKeys)) {
    singleCommentary <- commentaryKeys[i]
    elementsSplit <- strsplit(singleCommentary, ':')[[1]]
    matchID <- elementsSplit[3]
    teamID <- elementsSplit[4]
    results <- redisConnection$HMGET(key = singleCommentary,
                                     field = returnItems)
    names(results) <- returnItems
    results$possesiontime <- gsub(pattern = "%", replacement = "", results$possesiontime)
    resultsAsNums <- as.double(results)
    
    matchInfo <- redisConnection$HGETALL(key = paste0('csm:', competitionID, ':', seasonStarting, '/2018:', matchID))
    singleMatchInfo <- matchInfo[c(FALSE, TRUE)]
    names(singleMatchInfo) <- matchInfo[c(TRUE, FALSE)]
    
    scoreHome <- as.integer(singleMatchInfo$localteam_score)
    scoreAway <- as.integer(singleMatchInfo$visitorteam_score)
    homeAway <- names(which(singleMatchInfo == teamID))
    
    if (i == 4) {
      print(singleMatchInfo)
      print(paste0(scoreHome, '-', scoreAway))
    }
    if (scoreHome == scoreAway) {
      winLoseDraw <- "D"  
    } else {
      if (homeAway == "localteam_id") {
        if (scoreHome > scoreAway) {
          winLoseDraw <- "W"
        } else {
          winLoseDraw <- "L"
        }
      } else {
        if (scoreAway > scoreHome) {
          winLoseDraw <- "W"
        } else {
          winLoseDraw <- "L"
        }
      }
    }
    
    # Calculate team form
    print(i)
    print(matchID)
    print(teamID)
    print(commentaryKeys[i])
    print(which(matchData$localteam_id == teamID))
    
    singleTeamHome <- matchData[which(matchData$localteam_id == teamID), ]
    totalForm <- data.frame(stringsAsFactors = FALSE)
    for (j in 1:nrow(singleTeamHome)) {
      print(singleTeamHome)
      print(as.integer(singleTeamHome$localteam_score[j]))
      print(as.integer(singleTeamHome$visitorteam_score[j]))
      if (as.integer(singleTeamHome$localteam_score[j]) == as.integer(singleTeamHome$visitorteam_score[j])) {
        singleResult <- "D"
      } else if (as.integer(singleTeamHome$localteam_score[j]) > as.integer(singleTeamHome$visitorteam_score[j])) {
        singleResult <- "W"
      } else {
        singleResult <- "L"
      }
      totalForm <- rbind(totalForm, data.frame(date = singleTeamHome$formatted_date[j],
                                               form = singleResult,
                                               stringsAsFactors = FALSE))
    }
    
    
    singleTeamAway <- matchData[which(matchData$visitorteam_id == teamID), ]
    for (j in 1:nrow(singleTeamAway)) {
      if (as.integer(singleTeamAway$localteam_score[j]) == as.integer(singleTeamAway$visitorteam_score[j])) {
        singleResult <- "D"
      } else if (as.integer(singleTeamAway$localteam_score[j]) > as.integer(singleTeamAway$visitorteam_score[j])) {
        singleResult <- "L"
      } else {
        singleResult <- "W"
      }
      totalForm <- rbind(totalForm, data.frame(date = singleTeamAway$formatted_date[j],
                                               form = singleResult, 
                                               stringsAsFactors = FALSE))
    }
    indexSort <- sort.int(totalForm$date, decreasing = FALSE, index.return = TRUE)$ix
    totalForm <- totalForm[indexSort, ]
    form <- totalForm$form[1:3]
    form <- paste(form, collapse = '')

    singleItem <- data.frame(t(resultsAsNums), stringsAsFactors = FALSE)
    names(singleItem) <- returnItems
    singleItem$form <- form
    singleItem$res <- winLoseDraw
    if (i == 1) {
      totalData <- singleItem
    } else {
      totalData <- rbind(totalData, singleItem)
    }
  }
  return(totalData)
}
