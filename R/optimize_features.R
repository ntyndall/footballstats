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

  # Define the Season
  KEYS$SEASON <- 2017

  # Allowed Commentaries
  allowedCommentaries <-  c(
    'shots_total', 'shots_ongoal', 'fouls', 'corners',
    'possesiontime', 'yellowcards'
  )

  # SPlit by competition and subset
  allComps <- data.set$comp_id %>% unique
  subs.data <- data.set %>% subset(allComps[1] == data.set$comp_id)

  total.metrics <- data.frame(stringsAsFactors = FALSE)
  # Now look at subs.data
  for (i in 1:(subs.data %>% nrow)) {
    print(i)

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

    total.metrics %<>% rbind(matchMetrics)
  }

  # Change possesion to some integer
  total.metrics$possesiontime.a %<>%
    substr(1, total.metrics$possesiontime.a %>% nchar %>% `-`(1))
  total.metrics$possesiontime.h %<>%
    substr(1, total.metrics$possesiontime.h %>% nchar %>% `-`(1))

  DAYS <- 3
  # Now loop over all of total.metrics
  for (i in 2:(total.metrics %>% nrow)) {
    current.row <- total.metrics[i, ]
    smaller.metrics <- total.metrics[1:(i - 1), ]

    home <- current.row$localID
    away <- current.row$awayID

    homeMatched <- smaller.metrics$localID %>% `==`(home)
    awayMatched <- smaller.metrics$awayID %>% `==`(away)

    homeSum <- homeMatched %>% sum
    awaySum <- awayMatched %>% sum

    if (homeMatched %>% sum %>% `>=`(DAYS) && awayMatched %>% sum %>% `>`(DAYS)) {
      # do something here
    } else {
      next
    }
  }

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
