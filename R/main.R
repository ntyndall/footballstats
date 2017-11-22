#'
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
#' @export


main <- function()  {

  # Obtain API and sensitive key information
  KEYS <- footballstats::sensitive_keys()

  # Make a connection to redis for storing data
  rredis::redisConnect(
    host = 'localhost',
    port = 6379)
  rredis::redisSelect(1)

  # Load competitions and run the functionality below.
  competitions <- footballstats::acomp_info(
    KEYS = KEYS)
  #comps <- jsonlite::fromJSON(seasonIDs)

  # Subset the available competitions
  subsetCompetitions <- c('1102', '1204', '1205', '1229',
                          '1232', '1352', '1425', '1457')
  newCompetitions <- competitions[match(subsetCompetitions, competitions$id), ]

  # Loop over all competitions being analysed
  for (i in 1:nrow(newCompetitions)) {

    # Gather all information to be stored in Redis.
    print(paste0('Storing... ' , i, ' / ', nrow(newCompetitions), ' (',
                 newCompetitions$name[i], ' - ', newCompetitions$region[i], ').'))
    footballstats::add_all(
      competitionID = newCompetitions$id[i],
      updateData = FALSE,
      seasonStarting = 2017,
      KEYS = KEYS)

    # Send predicitons guessed correctly to Slack
    #  evaluatedPredictionsToSlack(competitionID = newCompetitions$id[i],
    #                              competitionName = newCompetitions$name[i])

    # Build a classifier with the current match data
    footballstats::classify_all(
      competitionID = newCompetitions$id[i],
      competitionName = newCompetitions$name[i],
      seasonStarting = 2017,
      returnItems = c('shots_total', 'shots_ongoal', 'fouls', 'corners', 'possesiontime', 'yellowcards', 'saves'),
      KEYS = KEYS)
  }
}
