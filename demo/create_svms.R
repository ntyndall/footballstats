
### Build commentary SVMs's
seasonStarting <- 2017
comps <- footballstats::allowed_comps()
footballstats::redis_con()

KEYS <<- footballstats::sensitive_keys(
  printToSlack = FALSE,
  printToScreen = FALSE,
  testing = FALSE,
  storePred = FALSE
)

cat(paste0(Sys.time(), ' | Recreating match data. \n'))
totalData <- data.frame(stringsAsFactors = FALSE)
for (i in 1:(comps %>% length)) {

  # Define the keys for each recreation
  KEYS$COMP <- comps[i]
  KEYS$SEASON <- seasonStarting

  # Recreate the match data
  matchData <- KEYS %>% footballstats::recreate_matchdata()
  totalData %<>% rbind(matchData)

  # Build league table
  matchData %>% footballstats::create_table()

  # Store positions on a weekly basis
  KEYS %>% footballstats::weekly_positions()
}


svm.models <- c()
for (i in 1:(comps %>% length)) {

  # Find the best result with different days
  singleLeague <- totalData[totalData$comp_id == comps[i], ]
  allTeams <- c(singleLeague$localteam_id, singleLeague$visitorteam_id) %>%
    unique
  KEYS$COMP <- comps[i]

  correctFeatures <- totTraining <- featureFrame <- data.frame(stringsAsFactors = FALSE)
  for (l in 1:(allTeams %>% length)) {

    # Initialise
    resSds <- resList <- weights <- c()
    commKey <- paste0('cmt_commentary:', KEYS$COMP)

    # Get the commentary names
    commentaryNames <- footballstats::dataScales$commentaries %>%
      strsplit(split = '[.]') %>%
      purrr::map(1) %>%
      purrr::flatten_chr() %>%
      unique

    # Take a single teamID to investigate
    teamID <- allTeams[l]

    commentaryKeys <- paste0(commKey, ':*:', teamID) %>%
      rredis::redisKeys()
    if (commentaryKeys %>% is.null) break else commentaryKeys %<>% as.character

    # If it does then continue on
    commentaryKeys <- KEYS %>% footballstats::order_commentaries(
      commentaryKeys = commentaryKeys
    ) %>% rev

    # Get all the relative positions
    matchIDs <- commentaryKeys %>%
      footballstats::flatt(y = 3)

    totalPositions <- data.frame(stringsAsFactors = FALSE)
    HAvec <- c()
    for (k in 1:(commentaryKeys %>% length)) {

      # Get the other team from the matchID
      bothIDs <- paste0(commKey, ':', matchIDs[k], ':*') %>%
        rredis::redisKeys() %>%
        footballstats::flatt(y = 4)

      # Need to make sure the current team is first!!
      if (teamID %>% `==`(bothIDs) %>% which %>% `==`(2)) bothIDs %<>% rev

      homeAway <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, ':', matchIDs[k]) %>%
        rredis::redisHMGet(
          fields = c('localteam_id', 'visitorteam_id')
        )

      homeAway <- teamID %>% `==`(homeAway) %>% which %>% mod(2) %>% as.integer
      HAvec %<>% c(homeAway)

      # Get the positions from
      positions <- KEYS %>% footballstats::feat_position(
        matchID = matchIDs[k],
        teamIDs = bothIDs
      )

      # Bind to a total data frame
      totalPositions %<>% rbind(positions)
    }

    # Get data frame of commentary metrics
    comMetrics <- commentaryKeys %>%
      footballstats::commentary_frame(
        commentaryNames = commentaryNames
      )

    # Take a running average every 4 games!
    dataRowsAdj <- comMetrics %>% nrow %>% `-`(3)

    allMean <- sapply(1:(commentaryNames %>% length), function(x) {
      sapply(2:dataRowsAdj, function(i) comMetrics[[commentaryNames[x]]][c(i:(i+3))] %>% mean)
    })
    allSD <- sapply(1:(commentaryNames %>% length), function(x) {
      sapply(2:dataRowsAdj, function(i) comMetrics[[commentaryNames[x]]][c(i:(i+3))] %>% sd)
    })

    elems <- c(1:(dataRowsAdj - 1))

    newF <- data.frame(
      currentPos = totalPositions$position.h[elems],
      otherPos = totalPositions$position.a[elems],
      homeaway = HAvec[elems]
    )

    myFeatures <- data.frame(allMean, allSD)
    names(myFeatures) <- c(commentaryNames %>% paste0('_mean'), commentaryNames %>% paste0('_sd'))

    featureFrame %<>% rbind(myFeatures)
    totTraining %<>% rbind(newF)
    correctFeatures %<>% rbind(comMetrics[elems, ])
  }

  # Now with the total training data, build and save an SVM per competition
  #totTraining %>% subset(totTraining %>% duplicated %>% `!`()) -> nn
  original <- totTraining
  varNames <- names(original) %>%
    paste(collapse = ' + ')

  # Build an SVM for each attribute
  uniqueSVM <- c()
  positionInt <- c(0, 5, 10, 15, 20, Inf)

  # Bucket up standings too
  totTraining$currentPos %<>% findInterval(positionInt)
  totTraining$otherPos %<>% findInterval(positionInt)

  for (k in 1:(commentaryNames %>% length)) {
    totTraining <- original
    sepInt <- if (k == 1 || k == 3) {
      c(0, 5, 10, 15, 20, 25, 30, Inf)
    } else if (k == 2 || k == 4 || k == 7) {
      c(0, 3, 6, 9, 12, Inf)
    } else if (k == 5) {
      c(0, 21, 41, 61, 81, 101)
    } else if (k == 6) {
      c(0, 2, 4, 6, Inf)
    }

    nuNames <- paste0(commentaryNames[k], c('_mean', '_sd'))
    newF <- data.frame(
      frame = featureFrame[[nuNames[1]]] %>% findInterval(sepInt) %>% as.factor,
      frametwo = featureFrame[[nuNames[2]]],
      framethree = correctFeatures[[commentaryNames[k]]] %>% findInterval(sepInt) %>% as.factor,
      stringsAsFactors = FALSE
    )
    names(newF) <- c(nuNames, commentaryNames[k])

    # Bind it on!
    totTraining %<>% cbind(newF)

    # Build an SVM
    commentarySVM <- e1071::svm(
      paste0(commentaryNames[k], ' ~ ', varNames) %>% as.formula,
      data = totTraining,
      kernel = 'radial'
    )

    # Set up prediction vector
    prd <- totTraining
    prd[[commentaryNames[k]]] <- NULL

    # Calculate prediction as a percentage
    res <- predict(commentarySVM, prd) %>%
      as.integer %>%
      `==`(totTraining[[commentaryNames[k]]]) %>%
      sum %>%
      `/`(totTraining %>% nrow)
    print(res)

    ###
    uniqueSVM %<>% c(commentarySVM %>% list)
  }
  names(uniqueSVM) <- commentaryNames

  ##
  svm.models %<>% c(uniqueSVM %>% list)
}

# Update list by competitionID
names(svm.models) <- comps

allsvms <- svm.models
save(allsvms, file = getwd() %>% paste0('/data/allsvms.rda'))



my_metrics <- function() {
  return(
    list(
      shots_total =  c(0, 5, 10, 15, 20, 25, 30, Inf),
      shots_ongoal = c(0, 3, 6, 9, 12, Inf),
      fouls =  c(0, 5, 10, 15, 20, 25, 30, Inf),
      corners = c(0, 3, 6, 9, 12, Inf),
      possesiontime = c(0, 21, 41, 61, 81, 101),
      yellowcards = c(0, 2, 4, 6, Inf),
      saves = c(0, 3, 6, 9, 12, Inf)
    )
  )
}

myMetrics -> cIntervals
save(cIntervals, file = getwd() %>% paste0('/data/cIntervals.rda'))
