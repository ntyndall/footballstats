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


generate_predictions <- function(KEYS, fixtureList, cMethod = "xgboost") {

  # Order the fixture list dataframe
  fixtureList <- fixtureList[fixtureList$formatted_date %>%
    as.Date(format = '%d.%m.%Y') %>%
    as.integer %>%
    order, ]

  # Load the appropriate data model
  datModel <- if (cMethod == "xgboost") {
    load(file = getwd() %>% paste0("/xgModel.rda"))
  } else {
    footballstats::nn
  }

  # Initialise arguments
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

    # Names of competing teams
    homeName <- singleFixture$localteam_name
    awayName <- singleFixture$visitorteam_name

    # Call in here
    predicted <- KEYS %>%
      footballstats::classify_method_selection(
        method = "xgboost",
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
    totalTxt %<>% c(
      paste0(
        singleFixture$localteam_id %>% blnk(),
        " `[", actualH, "] ", homeName, " vs. ", awayName, " [", actualA, "]` ",
        singleFixture$visitorteam_id %>% blnk()
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
      paste0('csdm_pred:', KEYS$COMP, ':', KEYS$SEASON, ':', month, ':', singleFixture$id) %>%
        KEYS$RED$HMSET(
          field = c("localteam", "visitorteam", "home", "away", "week", "prediction", "slack"),
          value = c(homeName, awayName, actualH, actualA, cDate, "-", "false")
        )

      # Also push the match ID to a list
      "all_predictions" %>% KEYS$RED$SADD(
        member = singleFixture$id
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
