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


generate_predictions <- function(KEYS, fixtureList, cMethod, datModel) {

  # Order the fixture list dataframe
  fixtureList <- fixtureList[fixtureList$zzz.date %>%
    as.Date(format = '%d.%m.%Y') %>%
    as.integer %>%
    order, ]

  # Initialise arguments
  analysed <- nAnalysed <- correct <- todaysDate <- 0
  totalTxt <- c()
  fixtureRow <- fixtureList %>% nrow

  # Parse important information
  KEYS$COMP <- fixtureList$zzz.compID %>% footballstats::prs_comp()
  KEYS$SEASON <- fixtureList$zzz.season %>% footballstats::prs_season()

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

    # Names of competing teams
    homeName <- singleFixture$home.team
    awayName <- singleFixture$away.team

    # Call in here
    predicted <- KEYS %>%
      footballstats::classify_method_selection(
        method = cMethod,
        singleFixture = singleFixture,
        datModel = datModel
      )

    # Increment objects
    analysed %<>% `+`(predicted$analysed)
    nAnalysed %<>% `+`(predicted$nAnalysed)

    # Skip if no predictions have been made
    if (predicted$nAnalysed %>% `==`(1)) next

    # Relabel predicted results
    actualH <- predicted$home
    actualA <- predicted$away

    # Take format of [2-1], split, convert and decide on win / lose / draw.
    fTime <- singleFixture$zzz.score
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
    cDate <- singleFixture$zzz.date
    if (todaysDate %>% `!=`(cDate)) {
      todaysDate <- cDate
      totalTxt %<>% c(paste0(' -- *', todaysDate, '* --'))
    }

    # Logs for console and for slack
    totalTxt %<>% c(
      paste0(
        singleFixture$home.id %>% blnk(),
        " `[", actualH, "] ", homeName, " vs. ", awayName, " [", actualA, "]` ",
        singleFixture$away.id %>% blnk()
      ) %>%
        as.character
    )

    # When making a prediction - store the guess for later
    if (KEYS$LOG_PRED) {
      month <- cDate %>%
        as.Date(format = '%d.%m.%Y') %>%
        as.character %>%
        strsplit(split = '-') %>%
        purrr::map(2) %>%
        purrr::flatten_chr() %>%
        as.integer

      # Create the hash of prediction information
      paste0('csdm_pred:', KEYS$COMP, ':', KEYS$SEASON, ':', month, ':', singleFixture$zzz.matchID) %>%
        KEYS$RED$HMSET(
          field = c("localteam", "visitorteam", "home", "away", "week", "prediction", "slack"),
          value = c(homeName, awayName, actualH, actualA, cDate, "-", "false")
        )

      # Also push the match ID to a list
      "all_predictions" %>% KEYS$RED$SADD(
        member = singleFixture$zzz.matchID
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
      ' : ', cMethod, ')_ :soccer: '
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
