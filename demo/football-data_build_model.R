
# Get all files
location <- "~/Documents/footballstats/inst/football-data/"

INCLUDE_ODDS <- TRUE

# Get all the file names
fNames <- location %>%
  paste0(location %>% list.files)

# Initialise variables
teamIDs <- list()
test.metrics <- total.metrics <- data.frame(stringsAsFactors = FALSE)

# Set up keys? 4 should be empty!
KEYS <- footballstats::keys_for_testing(
  dbnum = 4
)
# Set up the grids
GRIDS <- list(
  XG_BOUND = KEYS$XG_BOUND,
  DAYS = KEYS$DAYS,
  PARAM_GPOINTS = KEYS$PARAM_GPOINTS,
  PARAM_GBOUNDARY = KEYS$PARAM_GBOUNDARY,
  PARAM_DECAY = KEYS$PARAM_DECAY,
  PARAM_TOTALPER = KEYS$PARAM_TOTALPER
)

KEYS$RED$FLUSHDB()

# Loop over all the file names
for (i in 1:(fNames %>% length)) {
  cat("\n", i, "/", fNames %>% length, "\n\n")

  # Get full data set for a particular league
  full.data <- fNames[i] %>%
    read.table(
      sep = ",",
      header = T,
      stringsAsFactors = FALSE
    )

  full.data$Season <- "2018/2019"
  # Now get all the unique seasons
  uniqueSeasons <- full.data$Season %>%
    unique

  # Loop over all unique seasons
  for (j in 1:(uniqueSeasons %>% length)) {

    # Subset out a particular seasons
    season.data <- full.data %>%
      subset(Season %>% `==`(uniqueSeasons[j]) %>% `&`(Date %>% `!=`("")))

    # Now map data columns to correct headers!
    new.data <- season.data %>%
      footballstats::rename_columns(
        mapping = "footballdata"
      )

    # Get unique teams!
    uniqTeams <- new.data$home.team %>%
      c(new.data$away.team) %>%
      unique

    # Create new teamIDs
    for (k in 1:(uniqTeams %>% length)) {
      teamInc <- uniqTeams[k] %in% (teamIDs %>% names)
      if (teamIDs %>% length %>% `>`(0)) {
        if (teamInc %>% `!`()) {
          smallList <- teamIDs[[teamIDs %>% length]] %>% `+`(1) %>% list
          names(smallList) <- uniqTeams[k]
          teamIDs %<>% c(smallList)
        }
      } else {
        smallList <- list(1)
        names(smallList) <- uniqTeams[k]
        teamIDs %<>% c(smallList)
      }
    }

    # Add matchID's on
    new.data$zzz.matchID <- 1:(new.data %>% nrow) %>% as.character

    # Assign the new IDs
    new.data$home.id <- teamIDs[new.data$home.team] %>% as.character
    new.data$away.id <- teamIDs[new.data$away.team] %>% as.character

    # Order it
    new.data %<>% footballstats::order_matchdata(formatter = "%d/%m/%Y")

    # Assign season and competition
    KEYS$SEASON <- uniqueSeasons[j] %>%
      strsplit(split = "/") %>%
      purrr::flatten_chr() %>%
      `[`(1)
    KEYS$COMP <- i
    KEYS$TIL <- uniqTeams %>% length

    # Create table
    cat(paste0(Sys.time(), ' | Creating the league table ... \n'))
    KEYS %>% footballstats::create_table(
      matchData = new.data
    )

    # Store positions on a weekly basis
    cat(paste0(Sys.time(), ' | Storing weekly positions ... \n'))
    KEYS %>% footballstats::weekly_positions()

    # Loop over every row to calculate their positions
    pos.frame <- data.frame(stringsAsFactors = FALSE)
    for (k in 1:(new.data %>% nrow)) {
      pos.frame %<>% rbind(
        footballstats::feat_position(
          KEYS = KEYS,
          matchID = new.data$zzz.matchID[k],
          teamIDs = c(new.data$home.id[k], new.data$away.id[k]),
          matchDate = new.data$zzz.date[k]
        )
      )
    }

    # Length of the unique teams!
    new.data$zzz.til <- uniqTeams %>% length

    # Now append the positions on
    new.data %<>% cbind(pos.frame)

    # Append the results on
    new.data$zzz.result <- sapply(
      X = new.data$home.score %>% `-`(new.data$away.score),
      FUN = function(x) if (x %>% `>`(0)) "W" else if (x == 0) "D" else "L"
    )

    # Get the actual metrics required
    new.metrics <- new.data %>%
      sub_metrics(
        colNames = list(
          localID = "home.id",
          awayID = "away.id"
        ),
        GRIDS = GRIDS
      )

    # Also include odds as a feature!
    if (INCLUDE_ODDS) {
      odd.frme <- data.frame(stringsAsFactors = FALSE)
      for (o in 1:(new.metrics$data %>% nrow)) {
        odd.frme %<>% rbind(
          new.data %>%
            subset(zzz.matchID == new.metrics$matchIDs[o]) %>%
            dplyr::select(zzz.bet365Homewin, zzz.bet365Draw, zzz.bet365Awaywin)
        )
      }

      result.vec <- new.metrics$data$res
      new.metrics$data$res <- NULL

      if (odd.frme %>% nrow %>% `!=`(new.metrics$data %>% nrow)) {
        new.metrics$data %<>% cbind(
          data.frame(
            zzz.bet365Homewin = NA,
            zzz.bet365Draw = NA,
            zzz.bet365Awaywin = NA,
            stringsAsFactors = FALSE
          )
        )
      } else {
        new.metrics$data %<>% cbind(odd.frme)
      }

      new.metrics$data$res <- result.vec
    }

    # Bind it all onto one data frame
    if (uniqueSeasons[j] == "2018/2019") {
      test.metrics %<>% rbind(new.metrics$data)
    } else {
      total.metrics %<>% rbind(new.metrics$data)
    }
  }
}


training.metrics <- metrics$train
win.lose <- metrics$train

win.lose$res[win.lose$res == 'D'] <- 'L'


build_and_test_fd <- function(train.data, test.data, test.meta) {
  ACTUAL_RESULTS <- test.data$res

  train.data$res[train.data$res == 'D'] <- 'L'
  test.data$res[test.data$res == 'D'] <- 'L'
  TESTING_RESULTS <- test.data$res

  TRAINER <- train.data %>%
    mltools::scale_data()

  # tData <- TRAINER$data %>% mltools::scaled_to_discrete(boundLen = 4) %>% mltools::create_sparse(boundLen = 4)

  results.xgb <- gen_xgb(TRAINER$data, cName = 'res')

  testing.data <- test.data %>%
    footballstats::scale_data(TRAINER$scaler) %>%
    mltools::scaled_to_discrete(boundLen = 4)

  # GOT TO MAKE SURE BETWEEN 1 AND 5 (Scaler might not encapsulate everything)
  testing.data[testing.data %>% `==`(0)] <- 1
  testing.data %<>% mltools::create_sparse(boundLen = 4)

  PRED_RESULTS <- predict(
    results.xgb$model,
    testing.data
  )

  PRED_RESULTS[PRED_RESULTS %>% `==`(1)] <- 'W'
  PRED_RESULTS[PRED_RESULTS %>% `==`(0)] <- 'L'

  winnings <- calculate_winnings2(
    logicalVec = PRED_RESULTS %>% `==`(TESTING_RESULTS),
    PRED_RESULTS = PRED_RESULTS,
    ACTUAL_RESULTS = ACTUAL_RESULTS,
    odd.data = test.meta
  )

  print(winnings %>% sum)

  return(
    data.frame(
      actual = ACTUAL_RESULTS,
      predicted = PRED_RESULTS,
      winnings = winnings,
      stringsAsFactors = FALSE
    )
  )
}



new.win.lose <- win.lose %>% mltools::scale_data()

# Now build a model somewhere?!
results.xgb <- mltools::gen_xgb(new.win.lose$data, cName = "res")
results.nn <- mltools::gen_nn(new.win.lose$data, logs = TRUE)

footballstats::scale_data()



#originalRESULTS <- test.metrics$res



# Can I use test.metrics to get a feel for accuracy??
#test.metrics.scoring <- test.metrics %>%
#  dplyr::select(zzz.bet365Homewin, zzz.bet365Awaywin, zzz.bet365Draw, res)

test.metrics$zzz.bet365Awaywin <- test.metrics$zzz.bet365Draw <- test.metrics$zzz.bet365Homewin <- NULL
test.metrics$res[test.metrics$res == 'D'] <- 'L'
CURRENT_RESULTS <- test.metrics$res
test.metrics$res <- CURRENT_RESULTS




save(results.xgb, file = "~/Documents/currentXGB.rda")



# -- Assume bets are all £1

# Convert them all to numerics
test.metrics.scoring$zzz.bet365Homewin %<>% as.numeric
test.metrics.scoring$zzz.bet365Awaywin %<>% as.numeric
test.metrics.scoring$zzz.bet365Draw %<>% as.numeric
# Now I have the final results - see what the profit would be like
logicalVec <- FINALRESULTS %>% `==`(CURRENT_RESULTS)
notAnalysed <- 0
winnings <- c()
for (i in 1:(logicalVec %>% length)) {
  # If TRUE then won some money here


  if (logicalVec[i]) {
    # Find out if it's a win / lose correct prediction
    if (FINALRESULTS[i] == 'W') {
      winnings %<>% c(test.metrics.scoring$zzz.bet365Homewin[i])
    } else {
      # Must have lost / draw / lose?
      #if (test.metrics.scoring$zzz.bet365Awaywin[i] %>% `>`(2) %>% `&`(test.metrics.scoring$zzz.bet365Draw[i] %>% `>`(2))) {
        # What was the original bet?!?! Draw/ Lose?
        winnings %<>% c(
          if (originalRESULTS[i] == 'D') {
            test.metrics.scoring$zzz.bet365Draw[i] %>% `*`(0.5)
          } else {
            test.metrics.scoring$zzz.bet365Awaywin[i] %>% `*`(0.5)
          }
        )
      #} else {
      #  notAnalysed %<>% `+`(1)
      #}
    }
  }
}
# Number down
FINALRESULTS %>% length %>% `-`(winnings %>% length)

# Number up
winnings %>% sum








nas <- c()

for (i in 1:(win.lose %>% nrow)) {
  nas %<>% c(
    if (win.lose[i, ] %>% is.na %>% any) {
      FALSE
    } else {
      TRUE
    }
  )
}
win.lose %<>% subset(nas)


#result2[result2 == "L"] <- "W"
#result2[result2 == 1] <- "L"
#win.lose$res %>% `==`(result2) %>% sum


results$model -> testxgb
save(testxgb, file = "~/Documents/footballstats/temp/testxgb.rda")
results.xgb$model -> xgb.E0
save(xgb.E0, file = "~/Documents/footballstats/temp/xgbE0.rda")
