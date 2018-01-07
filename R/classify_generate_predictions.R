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


generate_predictions <- function(fixtureList, testing, SVMfit, dataScales,
                                 competitionName = "", correct = 0, totalTxt = c(),
                                 printToSlack = FALSE, KEYS, real = FALSE) {

  # Parse important information
  competitionID <- footballstats::prs_comp()
  seasonStarting <- footballstats::prs_season()

  # Set up slack details
  emojiHash <- footballstats::classify_emoji()

  # Loop over each fixture
  for (i in 1:nrow(fixtureList)) {
    singleFixture <- fixtureList[i, ]

    # Get team information from fixture data frame
    homeName <- singleFixture$localteam_name
    awayName <- singleFixture$visitorteam_name
    teamIDs <- c(singleFixture$localteam_id, singleFixture$visitorteam_id)

    # Get statistics for both teams
    resList <- c()
    for (j in 1:2) {
      bFrame <- data.frame(stringsAsFactors = FALSE)
      commentaryKeys <- paste0('cmt_commentary:', competitionID, ':*:', teamIDs[j]) %>%
        rredis::redisKeys() %>% as.character
      bFrame <- commentaryKeys %>% ord_keys() %>% get_av(commentaryNames = commentaryNames)
      avg <- apply(bFrame, 2, mean)

      avg %<>% get_frm(teamID = teamIDs[j], matchData = matchData)
      resList %<>% c(list(avg))
    }

    # Make the prediction based on scaled data frame results
    differ <- `-`(resList[[1]], resList[[2]])
    scled <- differ %>%
      as.data.frame %>%
      t %>%
      scale(
        center = dataScales$sMin,
        scale = dataScales$sMax - dataScales$sMin) %>%
      as.data.frame

    stats::predict(SVMDetails[[1]], scled)


    # Take format of [2-1], split, convert and decide on win / lose / draw.
    correct <- if (testing) {
      res <- singleFixture$ft_score %>%
        strsplit(split = '') %>%
        purrr::flatten_chr()
      res <- res[c(-1, -length(res))] %>%
        paste(collapse = '') %>%
        strsplit(split = '-') %>%
        purrr::flatten_chr() %>%
        as.numeric
      actual <- res %>%
        purrr::when(
          .[1] == .[2] ~ c('D', 'D'),
          .[1] > .[2] ~ c('W', 'L'),
          c('L', 'W'))
      if (actual[1] == pHome && actual[2] == pAway) correct + 1 else correct
    } else {
      correct + 1
    }

    # Set up emojis from the hash (feed in correct teamID)
    blnk <- function(tid) {
      tid %>%
        as.integer %>%
        emojiHash$find() %>%
        purrr::when(is.na(.) ~ ':blank-team:', ~ .) %>%
        return()
    }

    # Logs for console and for slack
    txt <- paste0('[', pHome, '] ', homeName, ' vs. ', awayName, ' [', pAway, ']') %>% as.character
    txtForSlack <- paste0(teamIDs[1] %>% blnk(), ' `', txt, '` ', teamIDs[2] %>% blnk()) %>% as.character
    totalTxt <- c(totalTxt, txtForSlack)

    # When making a prediction - store the guess for later
    if (real) {
      rredis::redisHMSet(
        key = paste0('c:', competitionID, ':pred:', singleFixture$id),
        values = list(
          home = pHome,
          away = pAway,
          week = singleFixture$week,
          slack = 'false'))
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
