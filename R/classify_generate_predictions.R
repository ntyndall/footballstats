#' @title generate_predictions
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
#' @export


generate_predictions <- function(competitionID, fixtureList, seasonStarting, testing, returnItems, subsetItems, SVMfit,
                                 matchFieldNames, competitionName = "", binList = NULL, correct = 0, totalTxt = c(),
                                 printToSlack = FALSE, KEYS, real = FALSE) {

  # Set up slack details
  emojiHash <- footballstats::classify_emoji()

  # Loop over each fixture
  for (i in 1:nrow(fixtureList)) {
    singleFixture <- fixtureList[i, ]

    homeName <- singleFixture$localteam_name
    awayName <- singleFixture$visitorteam_name

    # Remove form from the item subsetting
    forStatistics <- list('form', subsetItems) %>%
      purrr::when(.[[1]] %in% .[[2]] ~ .[[2]][-c(which(.[[1]] == .[[2]]))], ~ .[[2]])

    # Get home and away statistics
    fixtureAggregate <- footballstats::homeaway_stats(
      competitionID = competitionID,
      singleFixture = singleFixture,
      seasonStarting = seasonStarting,
      localVisitor = c('localteam_id', 'visitorteam_id'),
      returnItems = forStatistics,
      matchFieldNames = matchFieldNames,
      testing = testing)

    # Create the appropriate data structures for the SVM
    predictions <- as.character(sapply(1:2, function(k) {
      singleTeam <- fixtureAggregate[[k]][[1]] %>% as.integer %>% t() %>% data.frame()
      names(singleTeam) <- forStatistics

      # Map the current form to an integer based on rules in mapForm~
      singleTeam$form <- fixtureAggregate[[k]][[2]] %>% footballstats::form_to_int()

      # Only look at certain combinations if testing is enabled
        for (i in 1:length(subsetItems)) {
          vec <- singleTeam[[subsetItems[i]]]
          singleBin <- binList[[subsetItems[i]]]
          vec  <- findInterval(vec, singleBin) * (-1)
          singleTeam[[subsetItems[i]]] <- vec
        }
      stats::predict(SVMfit, singleTeam) %>% as.character
    }))

    # Predict scores now
    pHome <- predictions[1]
    pAway <- predictions[2]

    # Rules based on wrong outcomes!
    pHome <- c(pHome, pAway) %>%
      purrr::when(
        .[1] == 'D' && .[2] == 'W' ~ 'L',
        .[1] == 'D' && .[2] == 'L' ~ 'W',
        .[1])
    pAway <- c(pAway, pHome) %>%
      purrr::when(
        .[1] == 'D' && .[2] == 'L' ~ 'W',
        .[1] == 'D' && .[2] == 'W' ~ 'L',
        .[1])

    if (pHome == pAway) {
      pHome <- pAway <- 'D'
    }

    # Take format of [2-1], split, convert and decide on win / lose / draw.
    if (testing) {
      res <- (singleFixture$ft_score %>% strsplit(split = ''))[[1]]
      res <- (res[c(-1, -length(res))] %>% paste(collapse = '') %>%
        strsplit(split = '-'))[[1]] %>% as.numeric
      actual <- res %>%
        purrr::when(
          .[1] == .[2] ~ c('D', 'D'),
          .[1] > .[2] ~ c('W', 'L'),
          c('L', 'W'))
      correct <- if (actual[1] == pHome && actual[2] == pAway) correct + 1 else correct
    } else {
      correct <- correct + 1
    }

    # Set up emojis from the hash
    blnk <- function(inp) inp %>% purrr::when(is.na(.) ~ ':blank-team:', ~ .)
    homeEmoji <- singleFixture$localteam_id %>% as.integer %>% emojiHash$find() %>% blnk()
    awayEmoji <- singleFixture$visitorteam_id %>% as.integer %>% emojiHash$find() %>% blnk()

    # Logs for console and for slack
    txt <- as.character(paste0('[', pHome, '] ', homeName, ' vs. ', awayName, ' [', pAway, ']'))
    txtForSlack <- as.character(paste0(homeEmoji, ' `', txt, '` ', awayEmoji))
    totalTxt <- c(totalTxt, txtForSlack)

    # When making a prediction - store the guess for later
    if (real) {
      rredis::redisHMSet(
        key = paste0('c:', competitionID, ':pred:', singleFixture$id),
        values = list(home = pHome, away = pAway, week = singleFixture$week))
    }
    Sys.sleep(1)
  }

  if (printToSlack && real) { # nocov start
    slackr::slackrSetup(
      channel = '#results',
      api_token = KEYS$FS_SLACK)

    firstMsg <- paste0(
      ':soccer: _Reporting on results for week ',
      fixtureList$week[1], ' (', competitionName,
      ')_ :soccer: ')

    slackr::slackr_msg(
      txt = firstMsg,
      channel = '#results',
      api_token = KEYS$FS_SLACK,
      username = 'predictions')

    slackr::slackr_msg(
      txt = totalTxt,
      channel = '#results',
      api_token = KEYS$FS_SLACK,
      username = 'predictions')
  } # nocov end
  return(correct)
}
