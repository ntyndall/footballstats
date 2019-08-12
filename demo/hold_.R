predicted.raw.data <- new.data <- metrics$test.raw
new.metrics <- metrics$test

# # I may need to parse out incomplete cases!
# INCOMPLETE <- new.metrics %>% complete.cases() %>% `!`()
# if (INCOMPLETE %>% any) {
#   INCOMPLETE %>% which
# }

# new.metrics[INCOMPLETE %>% which, ]
#predicted.raw.data <- new.data %>%
#  subset(zzz.matchID %in% new.metrics$matchIDs)



ACTRESULTS <- new.metrics$res
test.data <- new.metrics


test.data$res[test.data$res == 'D'] <- 'L'
TOTEST <- test.data %>%
  mltools::scale_data() %>%
  `[[`("data") %>%
  mltools::scaled_to_discrete(boundLen = 4) %>%
  mltools::create_sparse(boundLen = 4)


# save(results.xgb, file = "~/Documents/currentXGB.rda")
FINALRESULTS <- predict(results.xgb$model, TOTEST)
FINALRESULTS[FINALRESULTS %>% `==`(1)] <- "W"
FINALRESULTS[FINALRESULTS %>% `==`(0)] <- "L"

# -- Assume bets are all £1
# Now I have the final results - see what the profit would be like
logicalVec <- FINALRESULTS %>% `==`(test.data$res)

logicalVec %>% sum %>% `/`(logicalVec %>% length)


winnings <- c()
for (i in 1:(logicalVec %>% length)) {
  # If TRUE then won some money here

  if (logicalVec[i]) {
    # Find out if it's a win / lose correct prediction
    if (FINALRESULTS[i] == 'W') {
      winnings %<>% c(predicted.raw.data$zzz.bet365Homewin[i] %>% `-`(1)) # This is the profit I would make!!!!!
    } else {
      # Must have lost / draw / lose?
      #if (test.metrics.scoring$zzz.bet365Awaywin[i] %>% `>`(2) %>% `&`(test.metrics.scoring$zzz.bet365Draw[i] %>% `>`(2))) {
      # What was the original bet?!?! Draw/ Lose?
      winnings %<>% c(
        if (ACTRESULTS[i] == 'D') {
          predicted.raw.data$zzz.bet365Draw[i] %>% `*`(0.5) %>% `-`(0.5)
        } else {
          predicted.raw.data$zzz.bet365Awaywin[i] %>% `*`(0.5) %>% `-`(0.5)
        }
      )
      #} else {
      #  notAnalysed %<>% `+`(1)
      #}
    }
  } else {
    winnings %<>% c(-1)
  }
}

# I need a data frame
resultsToPlot <- data.frame(
  date = predicted.raw.data$zzz.date,
  month = predicted.raw.data$zzz.date %>% format("%Y-%m"),
  predicted = FINALRESULTS,
  actual = test.data$res,
  correct = FINALRESULTS %>% `==`(test.data$res) %>% as.integer,
  winnings = winnings,
  season = predicted.raw.data$zzz.season,
  league = predicted.raw.data$zzz.division,
  stringsAsFactors = FALSE
)

# Now slice it in various ways
