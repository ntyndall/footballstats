#' @title Generate Predictions
#'
#' @description Another layer which can handle both normal fixture prediction
#'  and also predicting a test data set with known results.
#'  
#' @param fixtureList A data frame containing match localteam vs. visitorteam
#'  information which CAN include actual results if testing == TRUE.
#' @param testing A boolean value which decides whether to read the actual result
#'  of the match and compare with the classifier.
#' @param returnItems A vector of character values that hold the names of
#'  fields to be returned for the commentary statistics.
#' @param SVMfit An SVM object used as the classifier for predicting future
#'  matches.
#' @param binList A list of intervals defined by the min and max of a current
#'  statistic that can feed into the testing phase to see what variables are important.
#' @param correct An integer value that starts at 0 and is incremented for
#'  every match guessed correctly. Will default to number of matches predicted
#'  if not in testing phase.
#'
#' @return The agregated value of `correct`.
#'


generatePredictions <- function(fixtureList, testing, returnItems, subsetItems, SVMfit, 
                                binList = NULL, correct = 0, totalTxt = c()) {
  
  emojiHash <- lookUpSlackEmoji()
  
  # Loop over each fixture
  for (i in 1:nrow(fixtureList)) {
    singleFixture <- fixtureList[i, ]
    homeName <- singleFixture$localteam_name
    awayName <- singleFixture$visitorteam_name
    
    # Get home and away statistics
    fixtureAggregate <- getHomeAndAwayStats(singleFixture = singleFixture, 
                                            localVisitor = c('localteam_id', 'visitorteam_id'),
                                            returnItems = returnItems,
                                            testing = testing)

    # Create the appropriate data structures for the SVM
    predictions <- as.character(sapply(1:2, function(k) {
      singleTeam <- data.frame(t(as.integer(fixtureAggregate[[k]][[1]])))
      names(singleTeam) <- returnItems

      # Map the current form to an integer based on rules in mapForm~
      singleTeam$form <- mapFormToInteger(oldForms = fixtureAggregate[[k]][[2]])
      
      # Only look at certain combinations if testing is enabled
        for (i in 1:length(subsetItems)) {
          vec <- singleTeam[[subsetItems[i]]]
          singleBin <- binList[[subsetItems[i]]]
          vec  <- findInterval(vec, singleBin) * (-1)
          singleTeam[[subsetItems[i]]] <- vec
        }
      as.character(predict(SVMfit, singleTeam))
    }))
    
    # Predict scores now
    pHome <- predictions[1]
    pAway <- predictions[2]
    if (pHome == pAway) {
      pHome <- pAway <- 'D'
    }
    
    # Take format of [2-1], split, convert and decide on win / lose / draw.
    if (testing) {
      res <- strsplit(singleFixture$ft_score, '')[[1]]
      res <- as.numeric(strsplit(paste(res[c(-1, -length(res))], collapse = ''), '-')[[1]])
      actual <- res %>% purrr::when(.[1] == .[2] ~ c('D', 'D'),
                                    .[1] > .[2] ~ c('W', 'L'),
                                    ~ c('L', 'W'))
      if (actual[1] == pHome && actual[2] == pAway) {
        correct <- correct + 1
      }
    } else {
      correct <- correct + 1
    }
    
    # Set up emojis from the hash
    homeEmoji <- emojiHash$find(as.integer(singleFixture$localteam_id))
    awayEmoji <- emojiHash$find(as.integer(singleFixture$visitorteam_id))
    
    # Logs for console and for slack
    txt <- as.character(paste0('[', pHome, '] ', homeName, ' vs. ', awayName, ' [', pAway, ']'))
    txtForSlack <- as.character(paste0(homeEmjoji . ' `', txt, '` ', awayEmoji))
    totalTxt <- c(totalTxt, txtForSlack)
    cat(paste0(Sys.time(), ' : ', txt, '\n'))
    Sys.sleep(1)
  }
  
  if (!testing) {
    slackr::slackrSetup(channel = '#results', api_token = SLACK)
    firstMsg <- paste0(':soccer: _Reporting on results for week ', fixtureList$week[1], '_ :soccer: ')
    slackr::slackr_msg(txt = firstMsg, channel = '#results', api_token = SLACK, username = 'predictions')
    slackr::slackr_msg(txt = totalTxt, channel = '#results', api_token = SLACK, username = 'predictions')
  }
  return(correct)
}
