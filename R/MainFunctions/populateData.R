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


source(paste0(getwd(), '/Desktop/football-project/footballstats/R/MainFunctions/storeData.R'))
source(paste0(getwd(), '/Desktop/football-project/footballstats/R/UtilityFunctions/initialize.R'))

initialize(location = '~/Desktop/football-project/footballstats/R/', 
           redisHost = 'localhost',
           redisPort = 6379, 
           db = 1)

# Load competitions and run the functionality below. 
# (Figure out competition ID's with a single GET request first!)
competitions <- addCompetitionInfo()
comps <- jsonlite::fromJSON(seasonIDs)

# Gather all information to be stored in Redis.
mainController(redisConnection = redisConnection,
               competitionID = premiership, 
               seasonStarting = 2017,
               updateData = FALSE)

# Build a classifier with the current match data
buildGeneralClassifier(redisConnection = redisConnection,
                       competitionID = premiership,
                       seasonStarting = 2017,
                       returnItems = c('shots_total', 'shots_ongoal', 'fouls', 'corners', 'possesiontime', 'yellowcards', 'saves'))
