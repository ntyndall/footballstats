
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

  print(paste0('looking @ comp ', i, ' / ', comps %>% length))
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

      homeAway <- teamID %>% `==`(homeAway) %>% which %>% magrittr::mod(2) %>% as.integer
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

    # Set up function for calculating mean and SD of running 4 game average
    run_avg <- function(z = 'mean', dataset) {
      datNames <- dataset %>% names
      fn <- if (z == 'mean') mean else getFromNamespace(x = 'sd', ns = 'stats')
      return(
        sapply(
          X = 1:(datNames %>% length),
          FUN = function(x) {
            sapply(
              X = 2:dataRowsAdj,
              FUN = function(i) dataset %>% `[[`(datNames[x]) %>% `[`(c(i:(i+3))) %>% fn
            )
        })
      )
    }

    # Calculate mean and SD of 4 game average
    allMean <- run_avg(z = 'mean', dataset = comMetrics)
    allSD <- run_avg(z = 'sd', dataset = comMetrics)
    recentOp <- run_avg(z = 'mean', dataset = totalPositions %>% subset(select = 'position.a'))

    elems <- 2:dataRowsAdj

    # Do I need to offset the elems here?
    newF <- data.frame(
      currentPos = totalPositions$position.h[elems],
      otherPos = totalPositions$position.a[elems],
      recentOp = recentOp[ , 1],
      homeaway = HAvec[elems],
      stringsAsFactors = FALSE
    )

    myFeatures <- data.frame(allMean, allSD)
    names(myFeatures) <- c(commentaryNames %>% paste0('_mean'), commentaryNames %>% paste0('_sd'))

    featureFrame %<>% rbind(myFeatures)
    totTraining %<>% rbind(newF)
    correctFeatures %<>% rbind(comMetrics[elems, ])
  }

  # Now with the total training data, build and save an SVM per competition
  original <- totTraining
  varNames <- names(original) %>%
    paste(collapse = ' + ')

  uniqueSVM <- totRes <- c()
  for (k in 1:(commentaryNames %>% length)) {
    totTraining <- original

    nuNames <- paste0(commentaryNames[k], c('_mean', '_sd'))
    newF <- data.frame(
      frame = featureFrame[[nuNames[1]]],
      frametwo = featureFrame[[nuNames[2]]],
      framethree = correctFeatures[[commentaryNames[k]]],
      stringsAsFactors = FALSE
    )
    names(newF) <- c(nuNames, commentaryNames[k])

    # Bind it on and remove duplicated rows!
    totTraining %<>% cbind(newF)
    totTraining %<>% subset(totTraining %>% duplicated %>% `!`())

    # Build an SVM
    svmMethods <- c('radial', 'sigmoid', 'linear')
    newScore <- topScore <- 0
    for (m in 1:(svmMethods %>% length)) {
      #cat(m, '/ ')
      svmTune <- e1071::tune.svm(
        paste0(commentaryNames[k], ' ~ ', varNames) %>% as.formula,
        data = totTraining,
        kernel = svmMethods[m],
        sampling = 'fix',
        gamma = 2 %>% `^`(c(-8:4)),
        cost = 2 %>% `^`(c(-8:4))
      )
      newScore %<>% max(svmTune$best.performance)
      if (newScore > topScore) {
        topScore <- newScore
        bestModel <- svmTune$best.model
      }
    }

    # Create a list with SVMs for each commentary Name
    uniqueSVM %<>% c(bestModel %>% list)
  }

  # Create a total SVM model to hold the unique competition svm.
  names(uniqueSVM) <- commentaryNames
  svm.models %<>% c(uniqueSVM %>% list)
}

# Update list by competitionID
names(svm.models) <- comps
allsvms <- svm.models
save(allsvms, file = getwd() %>% paste0('/data/allsvms.rda'))


# ONLY FOR SINGLE!!
#
#svm.models$`1425` <- uniqueSVM
#allsvms <- svm.models
#
