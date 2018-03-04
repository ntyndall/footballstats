

# Optimization
uniqueComps <- totalData$comp_id %>% unique

standVar <- seq(from = 0.05, to = 0.5, by = 0.05)
footballstats::redis_con()
KEYS$LOGGING <- FALSE

for (i in 1:(standVar %>% length)) {
  KEYS$STAND <- standVar[i]

  totalCorrect <- 0
  for (j in 1:(uniqueComps %>% length)) {
    testData <- totalData %>% subset(totalData$comp_id == uniqueComps[j])
    correct <- footballstats::generate_predictions(
      fixtureList = testData,
      competitionName = 'test',
      KEYS = KEYS
    )
    per <- correct %>% `/`(testData %>% nrow) %>% scales::percent()
    cat('\n  ## ', per, ' accurate [', standVar[i], '] \n')
    totalCorrect %<>% `+`(correct)
  }

  per <- totalCorrect %>% `/`(totalData %>% nrow) %>% scales::percent()
  cat('\n Total! \n ## ', per, ' accurate [', standVar[i], '] \n')
}
