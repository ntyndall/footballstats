#' @title add_all
#'
#' @description A function that is called from a shell script to kick off
#'  the storing of important data and also any machine learning mechanisms
#'  for prediction.
#'
#' @details This main function is split into 3 main important components
#'  1) All libraries, global variables are loaded. Storing mechanisms
#'     that incorporate new ID keys are generated to store all useful
#'     information for classification / algorithms.
#'  2) Statistical models are built on the stored data and redis, future
#'     fixtures are obtained and built into the models.
#'  3) Predictions are made based on the current data and models and
#'     anything useful is sent via slack for easy analysis.
#'  Example queries carried out:
#'     ->   matches?comp_id=1204&from_date=01.01.2016&to_date=20.06.2016
#'     ->   team/9092
#'
#' @return Returns nothing.
#'
#' @export


add_all <- function(competitionID, updateData = FALSE,
                    seasonStarting, KEYS) { # nocov start

  # Begin finding match information
  dateFrom <- paste0('31.07.', seasonStarting)
  dateTo <- (Sys.Date() - 1) %>% footballstats::format_dates()

  # Add competition standing
  footballstats::acomp_standings(
    competition = competitionID,
    KEYS = KEYS)

  # Lookup request timings
  startingRequests <- 'requestLimit' %>% rredis::redisGet() %>% as.integer
  startingTime <- 'requestLimit' %>% rredis::redisTTL()

  # Add match information
  matches <- footballstats::amatch_info(
    competitionID = competitionID,
    dateFrom = dateFrom,
    dateTo = dateTo,
    seasonStarting = seasonStarting,
    updateData = updateData,
    KEYS = KEYS)
  print(paste0(Sys.time(), ': Matches complete.'))

  # Store predicted vs. real outcomes
  readyToAnalyse <- paste0('c:', competitionID, ':pred:*') %>% rredis::redisKeys()
  if (!(readyToAnalyse %>% is.null)) {
    footballstats::predict_vs_real(
      competitionID = competitionID,
      readyToAnalyseKey = readyToAnalyse,
      matches = matches)
  }

  # Add commentary information
  if (nrow(matches) > 0) {
    footballstats::acommentary_info(
      competitionID = competitionID,
      matchIDs = matches$id,
      localteam = matches$localteam_id,
      visitorteam = matches$visitorteam_id,
      KEYS = KEYS)
  }
  print(paste0(Sys.time(), ': Commentary complete.'))


  # Add event information
  if (nrow(matches) > 0) {
    footballstats::aevent_info(
      competitionID = competitionID,
      matchIDs = matches$id,
      matchEvents = matches$events)
  }
  print(paste0(Sys.time(), ': Events complete.'))

  # Add team information
  teamListLength <- 'analyseTeams' %>% rredis::redisLLen() %>% as.integer

  if (teamListLength > 0) {
    footballstats::ateam_info(
      competitionID = competitionID,
      teamListLength = teamListLength,
      updateData = updateData,
      KEYS = KEYS)
  }
  print(paste0(Sys.time(), ': Teams complete.'))

  # Add player information
  playerLength <- 'analysePlayers' %>% rredis::redisLLen() %>% as.integer

  if (playerLength > 0) {
    footballstats::aplayer_info(
      competitionID = competitionID,
      playerLength = playerLength,
      currentSeasonYear = seasonStarting,
      KEYS = KEYS)
  }
  print(paste0(Sys.time(), ': Players complete.'))

  # Count the number of GET requests made. 2 for competition standing and match information
  uniqueRequests <- 2
  totalRequests <- uniqueRequests + teamListLength + playerLength
  print(paste0(Sys.time(), ' : ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*'))
  print(paste0(Sys.time(), ' : Analysed ', totalRequests, ' unique GET requests.'))
  print(paste0(Sys.time(), ' : Analysed ', length(matches$events), ' matches/events.'))
  print(paste0(Sys.time(), ' : Analysed ', teamListLength, ' teams'))
  print(paste0(Sys.time(), ' : Analysed ', playerLength, ' players.'))
  cat('\n')
} # nocov end
