#' @title Predict Future Matches
#'
#' @description ...
#'  
#' @param competitionID An integer value denoting the competition ID.
#' @param seasonStarting An integer denoting the start year of the season.
#' @param returnItems A vector of character values that hold the names of
#'  fields to be returned for the commentary statistics.
#' @param SVMfit An SVM object used as the classifier for predicting future
#'  matches.
#'
#' @return ...


predictFutureMatches <- function(competitionID, seasonStarting, returnItems, SVMfit) {
  dateFrom <- formatDates(standardDateFormat = Sys.Date() + 1)
  dateTo <- formatDates(standardDateFormat = Sys.Date()  + 8)
  matchEndpoint <- paste0("/matches?comp_id=", competitionID, "&from_date=", dateFrom,
                          "&to_date=", dateTo, "&")
  fixtureList <- getGeneralData(endpoint = matchEndpoint)
  
  cat(paste0(Sys.time(), ' : About to report on results...\n'))
  for (i in 1:nrow(fixtureList)) {
    singleFixture <- fixtureList[i, ]
    home <- singleFixture$localteam_id
    homeName <- singleFixture$localteam_name
    away <- singleFixture$visitorteam_id
    awayName <- singleFixture$visitorteam_name
    
    homeCommentary <- as.character(redisConnection$KEYS(pattern = paste0('cmt*:', home)))
    awayCommentary <- as.character(redisConnection$KEYS(pattern = paste0('cmt*:', away)))
    
    currentStats <- NULL
    totalForm <- data.frame(stringsAsFactors = FALSE)
    for (j in 1:length(homeCommentary)) {

      results <- redisConnection$HMGET(key = homeCommentary[j], 
                                       field = returnItems)
      names(results) <- returnItems
      results$possesiontime <- gsub(pattern = "%", replacement = "", results$possesiontime)
      resultsAsNums <- as.double(results)
      
      # Also get the match ID's
      matchID <- strsplit(x = homeCommentary[j], split = ':')[[1]][3]

      redisKey <- paste0('csm:', competitionID, ':2017/2018:', matchID)
      matchFieldNames <- c('formatted_date', 'localteam_score', 'localteam_id', 'visitorteam_score', 'visitorteam_id')
      singleTeamHome <- redisConnection$HMGET(key = redisKey, field = matchFieldNames)
      names(singleTeamHome) <- matchFieldNames
    
      if (home == singleTeamHome$localteam_id) {
        res <- 'W'
      } else {
        res <- 'L'
      }
      
      if (singleTeamHome$localteam_score == singleTeamHome$visitorteam_score) {
        singleResult <- "D"
      } else if (singleTeamHome$localteam_score > singleTeamHome$visitorteam_score) {
        singleResult <- res
      } else {
        singleResult <- res
      }
      totalForm <- rbind(totalForm, data.frame(date = singleTeamHome$formatted_date,
                                               form = singleResult,
                                               stringsAsFactors = FALSE))

      
      if (j == 1) {
        currentStats <- resultsAsNums
      } else {
        currentStats <- (currentStats*(j-1) + resultsAsNums)/j
      }
      
    }
    indexSort <- sort.int(totalForm$date, decreasing = FALSE, index.return = TRUE)$ix
    totalForm <- totalForm[indexSort, ]
    form <- totalForm$form[1:3]
    form <- paste(form, collapse = '')
    
    singleHome <- data.frame(t(as.integer(currentStats)), stringsAsFactors = FALSE)
    names(singleHome) <- returnItems
    singleHome$form <- form
    
    
    
    # Same again but for away team.
    currentStats <- NULL
    totalForm <- data.frame(stringsAsFactors = FALSE)
    for (j in 1:length(awayCommentary)) {
      
      results <- redisConnection$HMGET(key = awayCommentary[j], 
                                       field = returnItems)
      names(results) <- returnItems
      results$possesiontime <- gsub(pattern = "%", replacement = "", results$possesiontime)
      resultsAsNums <- as.double(results)
      
      # Also get the match ID's
      matchID <- strsplit(x = awayCommentary[j], split = ':')[[1]][3]
      
      redisKey <- paste0('csm:', competitionID, ':2017/2018:', matchID)
      matchFieldNames <- c('formatted_date', 'localteam_score', 'localteam_id', 'visitorteam_score', 'visitorteam_id')
      singleTeamHome <- redisConnection$HMGET(key = redisKey, field = matchFieldNames)
      names(singleTeamHome) <- matchFieldNames
      
      if (away == singleTeamHome$localteam_id) {
        res <- 'W'
      } else {
        res <- 'L'
      }
      
      if (singleTeamHome$localteam_score == singleTeamHome$visitorteam_score) {
        singleResult <- "D"
      } else if (singleTeamHome$localteam_score > singleTeamHome$visitorteam_score) {
        singleResult <- res
      } else {
        singleResult <- res
      }
      totalForm <- rbind(totalForm, data.frame(date = singleTeamHome$formatted_date,
                                               form = singleResult,
                                               stringsAsFactors = FALSE))
      
      
      if (j == 1) {
        currentStats <- resultsAsNums
      } else {
        currentStats <- (currentStats*(j-1) + resultsAsNums)/j
      }
      
    }
    indexSort <- sort.int(totalForm$date, decreasing = FALSE, index.return = TRUE)$ix
    totalForm <- totalForm[indexSort, ]
    form <- totalForm$form[1:3]
    form <- paste(form, collapse = '')
   
    singleAway <- data.frame(t(as.integer(currentStats)), stringsAsFactors = FALSE)
    names(singleAway) <- returnItems
    singleAway$form <- form
    
   # Predict scores now
    predHome <- predict(fit, singleHome)
    predAway <- predict(fit, singleAway)
    
    if (predHome == predAway) {
      predHome <- predAway <- 'D'
    }
    txt <- as.character(paste0('[', predHome, '] ', homeName, ' vs. ', awayName, ' [', predAway, ']'))
    cat(paste0(Sys.time(), ' : ', txt, '\n'))
    Sys.sleep(1)
    
  }
  return(NULL)
}
