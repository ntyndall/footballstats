#' @title generate_predictions
#'
#' @description Another layer which can handle both normal fixture prediction
#'  and also predicting a test data set with known results.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[KEY]} :: \code{csdm_pred:{comp_id}:{season}:{month}:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#' @param fixtureList A data frame containing match localteam vs. visitorteam
#'  information which CAN include actual results if testing == TRUE.
#'
#' @return The agregated value of `correct`.
#'
#' @export


generate_predictions <- function(KEYS, fixtureList) {

  # Order the fixture list dataframe
  fixtureList <- fixtureList[fixtureList$formatted_date %>%
    as.Date(format = '%d.%m.%Y') %>%
    as.integer %>%
    order, ]

  # Initialise arguments
  dataScales <- footballstats::dataScales
  analysed <- nAnalysed <- correct <- todaysDate <- 0
  totalTxt <- c()
  fixtureRow <- fixtureList %>% nrow

  # Parse important information
  KEYS$COMP <- fixtureList$comp_id %>% footballstats::prs_comp()
  KEYS$SEASON <- fixtureList$season %>% footballstats::prs_season()

  # Set up slack details
  emojiHash <- footballstats::classify_emoji()

  # Set up progress bar
  progressBar <- utils::txtProgressBar(
    min = 0,
    max = fixtureRow,
    style = 3
  )

  # Loop over each fixture
  for (i in 1:fixtureRow) {
    # Update progress bar
    utils::setTxtProgressBar(progressBar, i)

    # Take a single row slice of the fixture list
    singleFixture <- fixtureList[i, ]

    # Get team information from fixture data frame
    matchID <- singleFixture$id %>% as.integer
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
      footballstats::project_commentaries(
        KEYS = KEYS,
        teamIDs = teamIDs,
        matchDate = singleFixture$formatted_date,
        matchID = matchID
      )
    )

    # Bind the form
    matchMetrics %<>% cbind(
      footballstats::project_form(
        KEYS = KEYS,
        teamIDs = teamIDs,
        currentID = matchID
      )
    )

    # Figure out the standings
    positions <- footballstats::feat_position(
      KEYS = KEYS,
      matchID = matchID,
      teamIDs = teamIDs,
      matchDate = singleFixture$formatted_date
    )

    matchMetrics %<>% cbind(
      data.frame(
        `position.h` = positions$position.h,
        `position.a` = positions$position.a,
        stringsAsFactors = FALSE
      )
    )

    # Go onto the next feature if any features arent present
    if (matchMetrics %>% is.na %>% any) {
      nAnalysed %<>% `+`(1)
      next
    } else {
      matchMetrics$matchID <- NULL
      analysed %<>% `+`(1)
    }

    # Scale the data as required
    scled <- matchMetrics %>%
      scale(
        center = dataScales$sMin,
        scale = dataScales$sMax - dataScales$sMin
      ) %>% as.data.frame

    # Make the prediction
    result <- neuralnet::compute(
      x = footballstats::nn$neural,
      covariate = scled
    )

    # Get the home team result
    resultsOrd <- c('D', 'L', 'W')
    actualH <- resultsOrd[result$net.result[1, ] %>% which.max]
    actualA <- if (actualH %>% `==`('W')) 'L' else if (actualH %>% `==`('L')) 'W' else 'D'

    # Take format of [2-1], split, convert and decide on win / lose / draw.
    fTime <- singleFixture$ft_score
    if (KEYS$TEST || fTime %>% `!=`('[-]')) {
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

    # Print out a new group of dates?
    cDate <- singleFixture$formatted_date
    if (todaysDate %>% `!=`(cDate)) {
      todaysDate <- cDate
      totalTxt %<>% c(paste0(' -- *', todaysDate, '* --'))
    }

    # Logs for console and for slack
    txt <- paste0('[', actualH, '] ', homeName, ' vs. ', awayName, ' [', actualA, ']') %>% as.character
    txtForSlack <- paste0(teamIDs[1] %>% blnk(), ' `', txt, '` ', teamIDs[2] %>% blnk()) %>% as.character
    totalTxt %<>% c(txtForSlack)

    # When making a prediction - store the guess for later
    if (KEYS$LOG_PRED) {
      month <- cDate %>%
        as.Date(format = '%d.%m.%Y') %>%
        as.character %>%
        strsplit(split = '-') %>%
        purrr::map(2) %>%
        purrr::flatten_chr() %>%
        as.integer

      rredis::redisHMSet(
        key = paste0('csdm_pred:', KEYS$COMP, ':', KEYS$SEASON, ':', month, ':', singleFixture$id),
        values = list(
          localteam = homeName,
          visitorteam = awayName,
          home = actualH,
          away = actualA,
          week = cDate,
          prediction = '-',
          slack = 'false'
        )
      )
    }
  }

  # Close the progress bar
  close(progressBar)

  if (KEYS$SLACK_PRNT && `!`(totalTxt %>% is.null)) { # nocov start
    slackr::slackrSetup(
      channel = '#results',
      api_token = KEYS$FS_SLACK
    )

    firstMsg <- paste0(
      ':soccer: _Reporting on results for week ',
      fixtureList$week[1], ' (', KEYS$COMP_NAME,
      ')_ :soccer: '
    )

    slackr::slackr_msg(
      txt = firstMsg,
      channel = '#results',
      api_token = KEYS$FS_SLACK,
      username = 'predictions'
    )

    slackr::slackr_msg(
      txt = totalTxt,
      channel = '#results',
      api_token = KEYS$FS_SLACK,
      username = 'predictions'
    )
  }

  # Print results to screen
  if (KEYS$LOGGING) {
    cat('Reporting on results! \n')
    print(totalTxt)
  } # nocov end

  return(
    list(
      correct = correct,
      notAnalysed = nAnalysed,
      analysed = analysed
    )
  )
}
