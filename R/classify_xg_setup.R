#' @title Classify XGBoost Setup
#'
#' @export


classify_xg_setup <- function(KEYS, singleFixture) {

  # Initialise list here
  predicted <- list(
    analysed = 0,
    nAnalysed = 0
  )

  # Get team information from fixture data frame
  matchID <- singleFixture$id %>% as.integer
  teamIDs <- c(singleFixture$localteam_id, singleFixture$visitorteam_id)
  sHome <- singleFixture$localteam_score
  sAway <- singleFixture$visitorteam_score

  # The result of the match
  res <- if (sHome > sAway) 'W' else if (sHome == sAway) 'D' else 'L'

  # Need a non-null frame to start with
  matchMetrics <- data.frame(
    matchID = matchID,
    date = singleFixture$formatted_date,
    localName = singleFixture$localteam_name,
    awayName = singleFixture$visitorteam_name,
    localID = teamIDs[1],
    awayID = teamIDs[2],
    localScore = sHome,
    awayScore = sAway,
    result = res,
    stringsAsFactors = FALSE
  )

  # Allowed Commentaries
  allowedCommentaries <-  c(
    'shots_total', 'shots_ongoal', 'fouls', 'corners',
    'possesiontime', 'yellowcards'
  )

  commKey <- paste0('cmt_commentary:', KEYS$COMP, ":")
  # Need to build a similar list of data frames...
  idTypes <- c('localID', 'awayID')
  all.inter.data <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:2) {
    currentID <- matchMetrics[[idTypes[i]]]
    cKeys <- commKey %>%
      paste0("*:", currentID) %>%
      rredis::redisKeys()

    if (cKeys %>% is.null) break else cKeys %<>% as.character

    # If it does then continue on
    ordKeys <- KEYS %>% footballstats::order_commentaries(
      commentaryKeys = cKeys
    ) %>% rev

    # Get the matchIDs
    matchIDs <- ordKeys %>%
      footballstats::flatt(y = 3)

    # Make sure matchID is next in the sequence
    subIDs <- matchIDs %>% `<`(matchID)

    # Subset IDs and keys
    ordKeys %<>% `[`(subIDs)
    matchIDs %<>% `[`(subIDs)

    # Construct basic key
    localOrAway <- c()
    for (j in 1:(matchIDs %>% length)) {
      localOrAway %<>% c(
        paste0("csm:", KEYS$COMP, ":", KEYS$SEASON, ':', matchIDs[j]) %>%
          rredis::redisHGet(field = "localteam_id") %>% as.character
      )
    }

    # May need to up this!
    DAYNUM <- 3

    localMatch <- localOrAway %>% `==`(currentID) %>% which %>% `[`(c(1:DAYNUM))
    awayMatch <- c(1:(localOrAway %>% length)) %>% setdiff(localMatch) %>% `[`(c(1:DAYNUM))

    # Now, what do I need in each data frame?!?!
    localComm <- ordKeys %>% `[`(localMatch)
    awayComm <- ordKeys %>% `[`(awayMatch)

    localMatchIDs <- matchIDs %>% `[`(localMatch)
    awayMatchIDs <- matchIDs %>% `[`(awayMatch)

    for (l in 1:2) {
      matchIDtypes <- if (l == 1) localMatchIDs else awayMatchIDs
      for (k in 1:DAYNUM) {
        baseResults <- paste0("csm:", KEYS$COMP, ":", KEYS$SEASON, ":", matchIDtypes[k]) %>%
          rredis::redisHMGet(
            fields = c("id", "formatted_date", "localteam_name", "localteam_id", "visitorteam_name", "visitorteam_id", "localteam_score", "visitorteam_score")
          ) %>% as.character

        # Get both home and away commentary keys
        homeawayKeys <- commKey %>% paste0(matchIDtypes[k], ":", c(baseResults[4], baseResults[6]))

        poss_to_int <- function(x) x %>% substr(1, x %>% nchar %>% `-`(1))

        for (m in 1:2) {
          cInfo <- homeawayKeys[m] %>%
            rredis::redisHMGet(fields = allowedCommentaries) %>%
            lapply(as.character)
          cLengths <- cInfo %>% lapply(length) %>% purrr::flatten_int() %>% `==`(0)
          if (cLengths %>% any) cInfo[cLengths %>% which] <- NA

          # Rename this new data frame and bind it to the metrics row
          cInfo %<>% data.frame(stringsAsFactors = FALSE)
          cInfo$possesiontime %<>% poss_to_int()
          names(cInfo) <- paste0(allowedCommentaries, if (m == 1) '.h' else '.a')
          if (m == 1) basic.stats <- cInfo else basic.stats %<>% cbind(cInfo)
        }

        # Format the date correctly
        newDate <- baseResults[2] %>% as.Date(format ='%d.%m.%Y')

        inter.data <- data.frame(
          matchID = baseResults[1],
          date = newDate,
          localName = baseResults[3],
          awayName= baseResults[5],
          localID = baseResults[4],
          awayID = baseResults[6],
          localScore = baseResults[7],
          awayScore = baseResults[8],
          result = if (baseResults[7] > baseResults[8]) 'W' else if (baseResults[7] == baseResults[8]) 'D' else 'L',
          stringsAsFactors = FALSE
        )

        inter.data %<>% cbind(basic.stats)

        # Need to also find the home and away positions
        positions <-  footballstats::feat_position(
          KEYS = KEYS,
          matchID = baseResults[1],
          teamIDs = c(baseResults[4], baseResults[6]),
          matchDate = newDate
        )

        inter.data$position.h <- positions$position.h
        inter.data$position.a <- positions$position.a
        inter.data$til <- KEYS$TIL

        # Bind everything on
        all.inter.data %<>% rbind(inter.data)
      }
    }
  }

  # NA'S EXIST
  if (all.inter.data %>% anyNA %>% `!`()) {
    # do calculations here (OPTIMIZE THESE VALUES ELSEWHERE!)
    result.dat <- all.inter.data %>% footballstats::optimize_calculation(
      day = 3,
      gridPoints = 4,
      gridBoundary = 0.1,
      decayFactor = 1,
      til = KEYS$TIL,
      totalPer = 0.5
    )

    # Now I need positions for the two current teams!!
    positions <- footballstats::feat_position(
      KEYS = KEYS,
      matchID = singleFixture$id,
      teamIDs = c(singleFixture$localteam_id, singleFixture$visitorteam_id),
      matchDate = singleFixture$formatted_date
    )

    # Append them on
    result.dat$`position.h` <- positions$`position.h` %>% `/`(KEYS$TIL)
    result.dat$`position.a` <- positions$`position.a` %>% `/`(KEYS$TIL)
    result.dat$res <-'U'
  }

  # If any are missing then return early
  if (all.inter.data %>% anyNA %>% `!`()) {
    predicted$nAnalysed %<>% `+`(1)
  } else {
    predicted$analysed %<>% `+`(1)

    # Scale the results
    scaled.results <- result.dat %>%
      footballstats::scale_data(
        dataScales = footballstats::xgScales
      )

    # Determine boundaries
    scaled.results %<>% footballstats::scaled_to_discrete(
      boundLen = 4
    )

    # Create a sparse matrix
    sparse.test <- scaled.results %>%
      footballstats::create_sparse(
        boundLen = 4
      )

    # Make the prediction
    result <- predict(footballstats::xgModel, sparse.test)

    # Get the home team result
    resultsOrd <- c('D', 'L', 'W')
    predicted$home <- resultsOrd[result %>% `+`(1)]
    predicted$away <- predicted$home %>% footballstats::other_score()
  }

  # Return single data frame row
  return(predicted)
}
