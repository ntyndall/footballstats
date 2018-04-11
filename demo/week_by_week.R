# Run through week by week
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

  KEYS$COMP <- 1205
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
}
