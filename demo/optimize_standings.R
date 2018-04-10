
# Build totalData elsewhere!!

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

  # Run to generate the fixtures (only the first 10 for now!)
  analysis <- KEYS %>% footballstats::generate_predictions(
    fixtureList = testData
  )

  per <- analysis$correct %>% `/`(testData %>% nrow %>% `-`(analysis$notAnalysed)) %>% scales::percent()
  cat('\n  ## ', singleLeague, '::', per, ' \n')
  #totalCorrect %<>% `+`(correct)

  # Save the total summary Statistics
  summaryStats %<>% rbind(
    data.frame(
      name = subComp$name,
      region = subComp$region,
      accuracy = per,
      stringsAsFactors = FALSE
    )
  )
}

per <- totalCorrect %>% `/`(totalData %>% nrow) %>% scales::percent()
cat('\n ## Total = ', per, ' accurate \n')













# Optimization
uniqueComps <- totalData$comp_id %>% unique

footballstats::redis_con()
KEYS %>% footballstats::acomp_info() -> competitions
competitions %<>% subset(competitions$id %in% footballstats::allowed_comps())
summaryStats <- data.frame(stringsAsFactors = FALSE)
KEYS$LOGGING <- FALSE

totalCorrect <- 0
for (j in 1:(uniqueComps %>% length)) {

  someDate <- Sys.Date() - 105

  testData <- totalData %>% subset(totalData$comp_id == uniqueComps[j])
  subComp <- competitions %>% subset(competitions$id == uniqueComps[j])
  singleLeague <- paste0(subComp$name, ' :: ', subComp$region)

  KEYS$COMP <- 1204
  KEYS$TIL <- KEYS$COMP %>% footballstats::teams_in_league()
  KEYS$COMP_NAME <- subComp$name

  for (j in 1:14) {
    print(j)
    KEYS$DATE_FROM <- someDate %>% `+`(j %>% `*`(7)) %>% footballstats::format_dates()
    KEYS$DATE_TO <- someDate %>% `+`(j %>% `*`(7) %>% `+`(6)) %>% footballstats::format_dates()

    testData <- paste0("/matches?comp_id=", KEYS$COMP, "&from_date=", KEYS$DATE_FROM, "&to_date=", KEYS$DATE_TO, "&") %>%
      footballstats::get_data(KEYS = KEYS)

    if (testData %>% is.null) next
    # Run to generate the fixtures (only the first 10 for now!)
    analysis <- KEYS %>% footballstats::generate_predictions(
      fixtureList = testData
    )

    per <- analysis$correct %>% `/`(testData %>% nrow %>% `-`(analysis$notAnalysed)) %>% scales::percent()
    cat('\n  ## ', j, '::', per, ' \n')



  }
  # Save the total summary Statistics
  summaryStats %<>% rbind(
    data.frame(
      name = subComp$name,
      region = subComp$region,
      accuracy = per,
      stringsAsFactors = FALSE
    )
  )
}





# Get fixtures
cat(paste0(Sys.time(), ' | About to report on results...\n'))
fixtureList <- if (KEYS$TEST) {
  footballstats::matchData[60:70, ]
} else { # nocov start
  paste0("/matches?comp_id=", KEYS$COMP, "&from_date=", KEYS$DATE_FROM, "&to_date=", KEYS$DATE_TO, "&") %>%
    footballstats::get_data(KEYS = KEYS)
} # nocov end

# Generate predictions based on actual fixtures!
if (fixtureList %>% is.null %>% `!`()) {
  numOfPredicted <- KEYS %>%
    footballstats::generate_predictions(
      fixtureList = fixtureList
    )
  predictions <- numOfPredicted$analysed
  cat(paste0(Sys.time(), ' | Predicted a total of ', numOfPredicted$analysed, ' matches. \n'))
  if (KEYS$TEST) cat(paste0(Sys.time(), ' | With ', numOfPredicted$correct, ' matches guessed correctly. \n'))
} else {
  predictions <- 0
  cat(paste0(Sys.time(), ' | No upcoming fixture in the next week! \n'))
