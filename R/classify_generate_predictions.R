#' @title generate_predictions
#'
#' @description Another layer which can handle both normal fixture prediction
#'  and also predicting a test data set with known results.
#'
#' @param fixtureList A data frame containing match localteam vs. visitorteam
#'  information which CAN include actual results if testing == TRUE.
#'
#' @return The agregated value of `correct`.
#'
#' @export


generate_predictions <- function(fixtureList, classifyModel, dataScales,
                                 competitionName = "", KEYS) {

  # Initialise arguments
  correct <- 0
  totalTxt <- c()

  # Parse important information
  competitionID <- fixtureList$comp_id %>% footballstats::prs_comp()
  seasonStarting <- fixtureList$season %>% footballstats::prs_season()

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
        rredis::redisKeys() %>%
        as.character %>%
        footballstats::ord_keys(
          competitionID = competitionID,
          seasonStarting = seasonStarting)

      # Get commentary names..
      cNames <- dataScales$sMax %>% names
      cNames <- cNames[c(1:(cNames %>% length %>% `-`(1)))]

      # Only calculate average - Can I do something more advanced here like a spline?
      bFrame <- commentaryKeys %>%
        footballstats::get_av(
          commentaryNames = cNames)
      avg <- apply(bFrame, 2, mean)

      # Get match IDs
      matchIDs <- commentaryKeys %>%
        strsplit(split = ':') %>%
        purrr::map(3) %>%
        purrr::flatten_chr()

      # Construct matchData like obect
      csmIDs <- paste0('csm:', competitionID, ':', seasonStarting, ':', matchIDs)

      cLen <- csmIDs %>% length
      matchData <- data.frame(stringsAsFactors = FALSE)
      for (k in (cLen - 2):cLen) {
        matchData %<>% rbind(csmIDs[k] %>%
          rredis::redisHGetAll() %>%
          as.data.frame)
      }

      avg %<>% footballstats::get_frm(
        teamID = teamIDs[j],
        matchData = matchData)

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

    #
    result <- neuralnet::compute(
      x = classifyModel,
      covariate = scled)

    # Get the home team result
    resultsOrd <- c('D', 'L', 'W')
    actualH <- resultsOrd[result$net.result[1, ] %>% which.max]
    actualA <- if (actualH %>% `==`('W')) 'L' else if (actualH %>% `==`('L')) 'W' else 'D'

    # Take format of [2-1], split, convert and decide on win / lose / draw.
    if (KEYS$TEST) {
      result <- singleFixture$ft_score %>%
        strsplit(split = '[[:punct:]]') %>%
        purrr::flatten_chr() %>%
        `[`(c(2:3)) %>%
        as.integer %>%
        purrr::when(.[1] == .[2] ~ 'D', .[1] > .[2] ~ 'W', 'L')
      if (result == actualH) correct %<>% `+`(1)
    } else {
      correct %<>% `+`(1)
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
    txt <- paste0('[', actualH, '] ', homeName, ' vs. ', awayName, ' [', actualA, ']') %>% as.character
    txtForSlack <- paste0(teamIDs[1] %>% blnk(), ' `', txt, '` ', teamIDs[2] %>% blnk()) %>% as.character
    totalTxt <- c(totalTxt, txtForSlack)

    # When making a prediction - store the guess for later
    if (KEYS$LOG_PRED) {
      rredis::redisHMSet(
        key = paste0('c:', competitionID, ':pred:', singleFixture$id),
        values = list(
          home = actualH,
          away = actualA,
          week = singleFixture$week,
          slack = 'false'))
    }
    Sys.sleep(1)
  }

  if (KEYS$SLACK_PRNT) { # nocov start
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
  } else {
    # Print results to screen
    print('Reporting on results!')
    print(totalTxt)
  } # nocov end
  return(correct)
}
