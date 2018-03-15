

# Optimization
uniqueComps <- totalData$comp_id %>% unique

footballstats::redis_con()
KEYS %>% footballstats::acomp_info() -> competitions
competitions %<>% subset(competitions$id %in% footballstats::allowed_comps())
summaryStats <- data.frame(stringsAsFactors = FALSE)
KEYS$LOGGING <- FALSE

totalCorrect <- 0
for (j in 1:(uniqueComps %>% length)) {
  testData <- totalData %>% subset(totalData$comp_id == uniqueComps[j])
  subComp <- competitions %>% subset(competitions$id == uniqueComps[j])
  singleLeague <- paste0(subComp$name, ' :: ', subComp$region)
  KEYS$COMP_NAME <- subComp$name

  if (j > 1) next
  # Run to generate the fixtures (only the first 10 for now!)
  correct <- KEYS %>% footballstats::generate_predictions(
    fixtureList = testData
  )

  per <- correct %>% `/`(testData %>% nrow) %>% scales::percent()
  cat('\n  ## ', singleLeague, '::', per, '{ DAY =', KEYS$DAYS, '/ STAND =', KEYS$STAND, '} \n')
  totalCorrect %<>% `+`(correct)

  # Save the total summary Statistics
  summaryStats %<>% rbind(
    data.frame(
      name = subComp$name,
      region = subComp$region,
      days = KEYS$DAYS,
      standVar = KEYS$STAND,
      accuracy = per,
      stringsAsFactors = FALSE
    )
  )
}

#per <- totalCorrect %>% `/`(totalData %>% nrow) %>% scales::percent()
#cat('\n ## Total = ', per, ' accurate [', standVar[i], '] \n')
