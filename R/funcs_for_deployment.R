#' @title  Run script to populate Redis.
#'
#' @details -- Summary of data structures stored in Redis --
#'
#'  1.1) Match information:
#'  -->  [csm]:{comp_id}:{season}:{match_id} - [HASH]
#'  1.2) Match exists?
#'  -->  [c_matchSetInfo]:{comp_id} - [SET]
#'  1.3) Team exists?
#'  -->  [c_teamSetInfo]:{comp_id} - [SET]
#'  1.4) Match commentary
#'  -->  [cmt_commentary]:{comp_id}:{match_id}:{team_id} - [HASH]
#'  1.5) Player statistics per match
#'  -->  [cmp]:{comp_id}:{match_id}:{player_id} - [HASH]
#'
#'  2.1) Events already analysed?
#'  -->  [c_eventInSet]:{comp_id} - [SET]
#'  2.2) Single event information:
#'  -->  [cme]:{comp_id}:{match_id}:{event_id} - [HASH]
#'
#'  3.1) Basic team information:
#'  -->  [ct_basic]:{comp_id}:{team_id} - [HASH]
#'  3.2) Team statistics:
#'  -->  [ct_stats]:{comp_id}:{team_id} - [HASH]
#'  3.3) Player information:
#'  -->  [ctp]:{comp_id}:{team_id}:{player_id} - [HASH]
#'
#'  4.1) Player statistics:
#'  -->  [ctps_[x]]:{comp_id}:{team_id}:{player_id}:{season} - [HASH]
#'       -->  where x = { club, club_intl, cups, national}
#'
#' @importFrom magrittr %>% %<>% %T>% %$%
#'
#' @export


collect_and_predict <- function(deployed = FALSE) { # nocov start

  # Get season starting year
  seasonStarting <- footballstats::start_season()

  # Obtain API and sensitive key information
  KEYS <- footballstats::sensitive_keys(
    printToSlack = TRUE,
    testing = FALSE,
    storePred = TRUE)

  # Make a connection to redis for storing data
  footballstats::redis_con()

  # Load competitions and run the functionality below.
  competitions <- KEYS %>% footballstats::acomp_info()

  # Subset the available competitions
  competitions <- competitions[footballstats::allowed_comps() %>% match(competitions$id), ]

  # Create the sink for output
  if (deployed) 'summary' %>% footballstats::create_sink()
  cat(' -- Beginning analysis -- \n\n\n')

  # Loop over all competitions being analysed
  for (i in 1:nrow(competitions)) {

    # Gather all information to be stored in Redis.
    cat(paste0(Sys.time(), ' | Storing ' , i, ' / ', nrow(competitions), ' (',
               competitions$name[i], ' - ', competitions$region[i], '). \n'))
    footballstats::add_all(
      competitionID = competitions$id[i],
      seasonStarting = seasonStarting,
      KEYS = KEYS)

    # Re-establish connection
    footballstats::redis_con()

    # Predict actual future results
    cat(paste0(Sys.time(), ' | Predicting actual upcoming fixtures. \n'))
    footballstats::predict_matches(
      competitionID = competitions$id[i],
      competitionName = competitions$name[i],
      KEYS = KEYS)

  }
} # nocov end

#' @title Analyse Players
#'
#' @description A function that analyses players only,
#'  this is to be run as a CRON job in deployment.
#'
#' @export


analyse_players <- function() { # nocov start
  # Get season starting year
  seasonStarting <- footballstats::start_season()

  # Obtain API and sensitive key information
  KEYS <- footballstats::sensitive_keys()

  # Make a connection to redis for storing data
  footballstats::redis_con()

  # Add player information
  playerLength <- 'analysePlayers' %>% rredis::redisLLen() %>% as.integer
  if (playerLength > 0) {
    footballstats::aplayer_info(
      playerLength = playerLength,
      currentSeasonYear = seasonStarting,
      KEYS = KEYS)
  }

  # Only complete - delete the analysePlayers key (if it exists..)
  if (rredis::redisExists(key = 'analysePlayers')) 'analysePlayers' %>% rredis::redisDelete()

} # nocov end

#' @title Analyse Match and Event
#'
#' @description A function that analyses matches and events only,
#'  this is to be run as a CRON job in deployment.
#'
#' @export


analyse_data <- function() { # nocov start
  # Get season starting year
  seasonStarting <- footballstats::start_season()

  # Obtain API and sensitive key information
  KEYS <- footballstats::sensitive_keys()

  # Make a connection to redis for storing data
  footballstats::redis_con()

  # Load competitions and run the functionality below.
  competitions <- KEYS %>% footballstats::acomp_info()

  # Subset the available competitions
  competitions <- competitions[footballstats::allowed_comps() %>% match(competitions$id), ]

  # Loop over all competitions being analysed
  for (i in 1:nrow(competitions)) {
    # Gather all information to be stored in Redis.
    cat(paste0('Storing... ' , i, ' / ', nrow(competitions), ' (',
               competitions$name[i], ' - ', competitions$region[i], '). \n'))

    footballstats::add_all(
      competitionID = competitions$id[i],
      seasonStarting = seasonStarting,
      KEYS = KEYS)
  }
} # nocov end
