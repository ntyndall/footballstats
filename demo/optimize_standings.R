
# Build totalData elsewhere!!

# Optimization
uniqueComps <- totalData$comp_id %>% unique

footballstats::redis_con()
KEYS %>% footballstats::acomp_info() -> competitions
competitions %<>% subset(competitions$id %in% footballstats::allowed_comps())
summaryStats <- data.frame(stringsAsFactors = FALSE)
KEYS$LOGGING <- FALSE
KEYS$TEST <- FALSE

startDate <- 17635 %>% as.Date(origin = '1970-01-01') %>% `+`(1)
totalCorrect <- 0
for (j in 1:(uniqueComps %>% length)) {

  testData <- totalData %>% subset(totalData$comp_id == uniqueComps[j])
  subComp <- competitions %>% subset(competitions$id == uniqueComps[j])
  singleLeague <- paste0(subComp$name, ' :: ', subComp$region)

  print(j)



  KEYS$DATE_FROM <- startDate %>% footballstats::format_dates()
  KEYS$DATE_TO <- Sys.Date() %>% `-`(1) %>% footballstats::format_dates()
  KEYS$COMP <- uniqueComps[j]
  KEYS$TIL <- KEYS$COMP %>% footballstats::teams_in_league()
  KEYS$COMP_NAME <- subComp$name


  testData <- paste0("/matches?comp_id=", KEYS$COMP, "&from_date=", KEYS$DATE_FROM, "&to_date=", KEYS$DATE_TO, "&") %>%
    footballstats::get_data(KEYS = KEYS)

  # Run to generate the fixtures (only the first 10 for now!)
  analysis <- KEYS %>% footballstats::generate_predictions(
    fixtureList = testData
  )

  if (analysis$analysed == 0) next

  per <- analysis$correct %>% `/`(testData %>% nrow %>% `-`(analysis$notAnalysed)) %>% scales::percent()
  cat('\n  ## ', singleLeague, '::', per, ' \n')
  totalCorrect %<>% `+`(correct)

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
