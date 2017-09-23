


generateTestStats <- function(binList, testData, SVMFit) {
  localVisitor <- c('localteam_id', 'visitorteam_id')
  
  correct <- 0
  for (i in 1:nrow(testData)) {
    singleFixture <- testData[i, ]
    homeName <- singleFixture$localteam_name
    awayName <- singleFixture$visitorteam_name
    
    fixtureAggregate <- lapply(1:2, function(j) {
      # Decide to analyse home team and then away team
      homeOrAway <- singleFixture[[localVisitor[j]]]
      commentary <- as.character(redisConnection$KEYS(pattern = paste0('cmt*:', homeOrAway)))
      
      # FOR TESTING DONT INCLUDE LAST COMMENTARY!!
      commentary <- commentary[1:(length(commentary) - 1)]
      
      # Determine the statistics of a commentary
      currentStats <- commentaryStatistics(commentary = commentary,
                                           returnItems = returnItems)
      
      # Also get the match ID's
      matchIDs <- sapply(1:length(commentary), function(k) {
        strsplit(x = commentary[[k]], split = ':')[[1]][3] 
      })
      
      # Determine forms from a vector of matches
      form <- getFormFromMatchIDs(matchIDs = matchIDs)
      list(currentStats, form)
    })
    

    names(binList) -> itemsForSVM
    # Create the appropriate data structures for the SVM
    predictions <- sapply(1:2, function(k) {
      singleTeam <- data.frame(t(as.integer(fixtureAggregate[[k]][[1]])))
      names(singleTeam) <- returnItems
      wld <- strsplit(fixtureAggregate[[k]][[2]], '')[[1]]
      singleTeam$form <- as.integer((sum(wld == "W")*2) + sum(wld == "D"))
      
      # Create endpoints for new grouping
      for (i in 1:length(itemsForSVM)) {
        vec <- singleTeam[[itemsForSVM[i]]]
        singleBin <- binList[[itemsForSVM[i]]]
        vec  <- findInterval(vec, singleBin) * (-1)
        singleTeam[[itemsForSVM[i]]] <- vec
      }
      as.character(predict(SVMFit, singleTeam))
    })
    
    
    if (predictions[1] == predictions[2]) {
      predictions <- c('D', 'D')
    }

    res <- strsplit(singleFixture$ft_score, '')[[1]]
    res <- res[c(-1, -length(res))]  
    res <- paste(res, collapse = '')
    res <- strsplit(res, '-')[[1]]
    homeS <- as.integer(res[1])
    awayS <- as.integer(res[2])
    if (homeS == awayS) {
      actual <- c('D', 'D')
    } else if (homeS > awayS) {
      actual <- c('W', 'L')
    } else {
      actual <- c('L', 'W')
    }

    #print(paste0(' [', predictions[1], '] ', homeName, ' vs ', awayName, ' [', predictions[2], '] '))
    if (actual[1] == predictions[1] && actual[2] == predictions[2]) {
      correct <- correct + 1
    }
  }
  return(correct)
}
  