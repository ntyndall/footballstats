#' @title Optimize Features
#'
#' @description A function used to optimize data set features.
#'
#' @details This function takes a number of parameters, and goes through
#'  each unique combination in order to try and optimize the feature sets
#'  based on the total data used.
#'
#' @export


optimize_features <- function(data.set, team = 100.0, player = 0.0, days = 4, decay = 0.0, folds = 10) {

  # Connect redis
  footballstats::redis_con()

  # Define the Season
  KEYS$SEASON <- 2017

  # Allowed Commentaries
  allowedCommentaries <-  c(
    'shots_total', 'shots_ongoal', 'fouls', 'corners',
    'possesiontime', 'yellowcards'
  )

  # SPlit by competition and subset
  allComps <- data.set$comp_id %>% unique


  total.metrics <- data.frame(stringsAsFactors = FALSE)
  for (comp in 1:(allComps %>% length)) {

    cat(' ##', comp, '/', allComps %>% length, '\n')
    subs.data <- data.set %>% subset(allComps[comp] == data.set$comp_id)
    # Now look at subs.data
    for (i in 1:(subs.data %>% nrow)) {
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

      # Get commentary information
      for (i in 1:2) {
        cInfo <- paste0('cmt_commentary:', KEYS$COMP, ':', matchID, ':', teamIDs[i]) %>%
          rredis::redisHMGet(fields = allowedCommentaries) %>%
          lapply(as.character)

        # Any items that don't exist in redis will have zero length, replace with NA
        cLengths <- cInfo %>% lapply(length) %>% purrr::flatten_int() %>% `==`(0)
        if (cLengths %>% any) cInfo[cLengths %>% which] <- NA

        # Rename this new data frame and bind it to the metrics row
        cInfo %<>% data.frame(stringsAsFactors = FALSE)
        names(cInfo) <- paste0(allowedCommentaries, if (i == 1) '.h' else '.a')
        matchMetrics %<>% cbind(cInfo)
      }

      # Bind the positions on
      matchMetrics %<>% cbind(
        footballstats::feat_position(
          KEYS = KEYS,
          matchID = matchID,
          teamIDs = teamIDs,
          matchDate = single.row$formatted_date
        )
      )

      matchMetrics %<>% cbind(
        data.frame(
          til = KEYS$TIL,
          stringsAsFactors = FALSE
        )
      )
      total.metrics %<>% rbind(matchMetrics)
    }
  }
  # Change possesion to some integer
  total.metrics$possesiontime.a %<>%
    substr(1, total.metrics$possesiontime.a %>% nchar %>% `-`(1))
  total.metrics$possesiontime.h %<>%
    substr(1, total.metrics$possesiontime.h %>% nchar %>% `-`(1))

  # Start to optimize this data set
  total.metrics %>%
    footballstats::optimize_variables()

  # Get player strength once developed
  if (FALSE) {
    pStrength <- matchID %>%
      footballstats::player_strength()
  }

  # Carry out Neural network CV
  if (FALSE) {
    for (i in 1:folds) {
      footballstats::neural_network()
    }
  }

}
