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


generate_predictions <- function(fixtureList, competitionName = "", KEYS) {

  # Initialise arguments
  dataScales <- footballstats::dataScales
  correct <- 0
  totalTxt <- c()

  # Parse important information
  competitionID <- fixtureList$comp_id %>% footballstats::prs_comp()
  seasonStarting <- fixtureList$season %>% footballstats::prs_season()

  # Set up slack details
  emojiHash <- footballstats::classify_emoji()

  # Just query for the standings here!
  # Need a condition for testing case!
  #
  #

  # Set up progress bar
  progressBar <- utils::txtProgressBar(
    min = 0,
    max = nrow(fixtureList),
    style = 3)

  # Loop over each fixture
  for (i in 1:nrow(fixtureList)) {
    utils::setTxtProgressBar(progressBar, i)
    singleFixture <- fixtureList[i, ]

    # Get team information from fixture data frame
    matchID <- singleFixture$id
    homeName <- singleFixture$localteam_name
    awayName <- singleFixture$visitorteam_name
    teamIDs <- c(singleFixture$localteam_id, singleFixture$visitorteam_id)

    # Need a non-null frame to start with
    matchMetrics <- data.frame(
      matchID = matchID,
      stringsAsFactors = FALSE
    )

    # Bind the commentaries together
    matchMetrics %<>% cbind(
      project_commentaries(
        competitionID = competitionID,
        seasonStarting = seasonStarting,
        teamIDs = teamIDs
      )
    )

    # Bind the form
    matchMetrics %<>% cbind(
      footballstats::project_form(
        competitionID = competitionID,
        seasonStarting = seasonStarting,
        teamIDs = teamIDs
      )
    )

    # Need to get a projection of convince-ability too....
    matchMetrics %<>% cbind(
      footballstats::project_convince(
        competitionID = competitionID,
        seasonStarting = seasonStarting,
        teamIDs = teamIDs
      )
    )

    # Go onto the next feature if any features arent present
    if (matchMetrics %>% is.na %>% any) next else matchMetrics$matchID <- NULL

    # Scale the data as required
    scled <- matchMetrics %>%
      scale(
        center = dataScales$sMin,
        scale = dataScales$sMax - dataScales$sMin) %>%
      as.data.frame

    # Make the prediction
    result <- neuralnet::compute(
      x = footballstats::nn,
      covariate = scled)

    # Get the home team result
    resultsOrd <- c('D', 'L', 'W')
    actualH <- resultsOrd[result$net.result[1, ] %>% which.max]
    actualA <- if (actualH %>% `==`('W')) 'L' else if (actualH %>% `==`('L')) 'W' else 'D'

    # Take format of [2-1], split, convert and decide on win / lose / draw.
    fTime <- singleFixture$ft_score
    if (KEYS$TEST || fTime %>% `==`('[-]')) {
      result <- fTime %>%
        footballstats::prs_ftscore() %>%
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
    Sys.sleep(0.5)
  }

  # Close the progress bar
  close(progressBar)

  if (KEYS$SLACK_PRNT && `!`(totalTxt %>% is.null)) { # nocov start
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
    cat('Reporting on results! \n')
    print(totalTxt)
  } # nocov end
  return(correct)
}
