#' @title Optimize Features
#'
#' @description A function used to optimize data set features.
#'
#' @details This function takes a number of parameters, and goes through
#'  each unique combination in order to try and optimize the feature sets
#'  based on the total data used.
#'
#' @export


optimize_features <- function(optimizeModels = FALSE) {

  # Optimize creates a grid, whereas a single run just saves a model
  GRIDS <- if (optimizeModels) {
    footballstats::initialize_ml_grid()
  } else {
    list(
      DAYS = 3,
      GRID_PTS = 8,
      GRID_BOUND = 0.2,
      DECAY = 5000,
      TOTAL_PERC = 1,
      NN_REP = 1,
      NN_THRESH = 0.005,
      XG_ROUNDS = 100,
      XG_DEPTH = 10,
      XG_ETA = 0.2,
      XG_GAMMA = 2,
      XG_BOUNDARY = 4
    )
  }

  # Set up keys (Use the production DB)
  KEYS <- footballstats::keys_for_testing(
    dbnum = 1
  )

  # Define the Season
  KEYS$SEASON <- 2017

  # Allowed Commentaries
  allowedCommentaries <-  c(
    'shots_total', 'shots_ongoal', 'fouls', 'corners',
    'possesiontime', 'yellowcards'
  )

  # Get all competitions
  allComps <- footballstats::allowed_comps()
  data.set <- footballstats::data.2017

  total.metrics <- data.frame(stringsAsFactors = FALSE)
  for (comp in 1:(allComps %>% length)) {

    cat(' \n ##', comp, '/', allComps %>% length, '\n')
    subs.data <- data.set %>% subset(allComps[comp] == data.set$comp_id)

    # Set up a progress bar here
    pb <- utils::txtProgressBar(
      min = 0,
      max = subs.data %>% nrow,
      style = 3
    )

    # Now look at subs.data
    for (i in 1:(subs.data %>% nrow)) {

      # Update the progress bar
      utils::setTxtProgressBar(
        pb = pb,
        value = i
      )

      # Take a single row slice of the fixture list
      single.row <- subs.data[i, ]

      # Set up new keys
      KEYS$COMP <- single.row$comp_id
      KEYS$TIL <- KEYS$COMP %>% footballstats::teams_in_league()

      # Get team information from fixture data frame
      matchID <- single.row$id %>% as.integer
      homeName <- single.row$localteam_name
      awayName <- single.row$visitorteam_name
      teamIDs <- c(single.row$localteam_id, single.row$visitorteam_id)

      # Scores for home and away
      sHome <- single.row$localteam_score
      sAway <- single.row$visitorteam_score

      # The result of the match
      res <- if (sHome > sAway) 'W' else if (sHome == sAway) 'D' else 'L'

      # Need a non-null frame to start with
      matchMetrics <- data.frame(
        matchID = matchID,
        date = single.row$formatted_date,
        localName = single.row$localteam_name,
        awayName = single.row$visitorteam_name,
        localID = teamIDs[1],
        awayID = teamIDs[2],
        localScore = sHome,
        awayScore = sAway,
        result = res,
        stringsAsFactors = FALSE
      )

      # Add commentary information on
      cInfo <- lapply(
        X = paste0("csmt_commentary:", KEYS$COMP, ":", KEYS$SEASON, ":", matchID, ":", teamIDs),
        FUN = function(x) x %>% footballstats::commentary_from_redis(returnItems = allowedCommentaries)
      ) %>%
        purrr::flatten_dbl()

      # Go onto next if cInfo isn't long enough
      if (cInfo %>% length %>% `!=`(12)) next

      # Create data frame of cInfo and append on
      names(cInfo) <- sapply(c(".h", ".a"), function(x) allowedCommentaries %>% paste0(x)) %>% as.character
      matchMetrics %<>% cbind(
        cInfo %>%
          data.frame(stringsAsFactors = FALSE) %>%
          t
        )

      # Bind the positions on
      matchMetrics %<>% cbind(
        footballstats::feat_position(
          KEYS = KEYS,
          matchID = matchID,
          teamIDs = teamIDs,
          matchDate = single.row$formatted_date
        )
      )

      # Bind the teams in league on, to do analysis later
      matchMetrics %<>% cbind(
        data.frame(
          til = KEYS$TIL,
          stringsAsFactors = FALSE
        )
      )
      total.metrics %<>% rbind(matchMetrics)
    }
  }

  # Data has been saved for easy access
  # total.metrics <- footballstats::total.metrics

  # Start to optimize this data set
  myres <- total.metrics %>%
    footballstats::optimize_variables(
      optimizeModels = FALSE,
      GRIDS = GRIDS,
      types = "neuralnetwork",
      saveModels = "neuralnetwork"
    )
}
