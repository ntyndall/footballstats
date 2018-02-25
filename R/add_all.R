#' @title Add All Information
#'
#' @description A function that is called from a shell script to kick off
#'  the storing of important data and also any machine learning mechanisms
#'  for prediction.
#'
#' @details This main function is split into 3 main important components
#'  \itemize{
#'    \item{
#'      All libraries, global variables are loaded. Storing mechanisms
#'      that incorporate new ID keys are generated to store all useful
#'      information for classification / algorithms.
#'    }
#'    \item{
#'      Statistical models are built on the stored data and redis, future
#'      fixtures are obtained and built into the models.
#'    }
#'    \item{
#'      Predictions are made based on the current data and models and
#'      anything useful is sent via slack for easy analysis.
#'    }
#'  }
#'
#' @param competitionID An integer ID of the competition defined by the API.
#' @param seasonStarting An integer of _Y_ format that defines the start of the
#'  season.
#' @param KEYS A list containing options such as testing / prediction
#'  options and also the API query information such as url.
#'
#' @return Returns nothing.
#'
#' @export


add_all <- function(KEYS) { # nocov start

  # Add competition standing
  KEYS%>% footballstats::acomp_standings()

  # Lookup request timings
  startingRequests <- 'requestLimit' %>% rredis::redisGet() %>% as.integer
  startingTime <- 'requestLimit' %>% rredis::redisTTL()

  # Add match information
  cat(paste0(Sys.time(), ' | Matches ...'))
  matches <- KEYS %>% footballstats::amatch_info()
  cat(' complete \n')

  # Store predicted vs. real outcomes
  readyToAnalyse <- paste0('csdm_pred:', KEYS$COMP, ':', KEYS$SEASON, ':*') %>% rredis::redisKeys()
  if (!(readyToAnalyse %>% is.null)) {
    KEYS %>% footballstats::predict_vs_real(
      readyToAnalyse = readyToAnalyse,
      matches = matches
    )
  }

  # Add commentary information
  cat(paste0(Sys.time(), ' | Commentary ...'))
  if (matches %>% nrow %>% `>`(0)) {
    KEYS %>% footballstats::acommentary_info(
      matchIDs = matches$id,
      localteam = matches$localteam_id,
      visitorteam = matches$visitorteam_id
    )
  }
  cat(' complete \n')

  # Add event information
  cat(paste0(Sys.time(), ' | Events ...'))
  if (matches %>% nrow %>% `>`(0)) {
    KEYS %>% footballstats::aevent_info(
      matchIDs = matches$id,
      matchEvents = matches$events
    )
  }
  cat(' complete \n')

  # Add team information
  teamListLength <- 'analyseTeams' %>% rredis::redisLLen() %>% as.integer
  plBef <- 'analysePlayers' %>% rredis::redisLLen() %>% as.integer

  cat(paste0(Sys.time(), ' | Teams ... \n'))
  # Add the team information
  if (teamListLength > 0) {
    KEYS %>% footballstats::ateam_info(
      teamListLength = teamListLength,
    )
  }
  cat(' complete. \n\n')
  plAft <- 'analysePlayers' %>% rredis::redisLLen() %>% as.integer

  # Count the number of GET requests made. 2 for competition standing and match information
  uniqueRequests <- 2
  totalRequests <- uniqueRequests + teamListLength
  cat(paste0(Sys.time(), ' . ----------------{-S-U-M-M-A-R-Y-}------------------ \n'))
  cat(paste0(Sys.time(), ' | Analysed ', totalRequests, ' unique GET requests. \n'))
  cat(paste0(Sys.time(), ' | Analysed ', length(matches$events), ' matches/events. \n'))
  cat(paste0(Sys.time(), ' | Analysed ', teamListLength, ' teams. \n'))
  cat(paste0(Sys.time(), ' | Players to be analysed : ', plBef, ' -> ', plAft, '. \n'))
  cat(paste0(Sys.time(), ' ` -------------------------------------------------- \n\n'))
} # nocov end
