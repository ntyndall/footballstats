# Run through week by week

# Set up KEYS
KEYS <- footballstats::sensitive_keys(
  printToSlack = FALSE,
  printToScreen = FALSE,
  testing = FALSE,
  storePred = FALSE
)
KEYS$SEASON <- 2017
KEYS$LOGGING <- FALSE

# Get competition information
footballstats::redis_con()
KEYS %>% footballstats::acomp_info() -> competitions
competitions %<>% subset(competitions$id %in% footballstats::allowed_comps())
uniqueComps <- competitions$id

# Choose a start date
startDate <- '31.08.2017' %>%
  as.Date('%d.%m.%Y')

# Calculate number of week loops to go through
numLoops <- Sys.Date() %>%
  `-`(startDate) %>%
  as.integer %>%
  `/`(7) %>%
  floor %>%
  `-`(1)

# Initialise week data frame
weekByWeek <- data.frame(stringsAsFactors = FALSE)

# Loop each week
for (i in 6:numLoops) {

  avgPer <- totalCorrect <- totalAnalysed <- 0

  # Set up query keys
  KEYS$DATE_FROM <- startDate %>% `+`(i %>% `*`(7)) %>% footballstats::format_dates()
  KEYS$DATE_TO <- startDate %>% `+`(i %>% `*`(7) %>% `+`(6)) %>% footballstats::format_dates()

  for (j in 1:(uniqueComps %>% length)) {
    if (j %>% `==`(uniqueComps %>% length)) cat(j) else cat(j, '...')
    KEYS$COMP <- uniqueComps[j]
    KEYS$TIL <- KEYS$COMP %>% footballstats::teams_in_league()

    testData <- paste0("/matches?comp_id=", KEYS$COMP, "&from_date=", KEYS$DATE_FROM, "&to_date=", KEYS$DATE_TO, "&") %>%
      footballstats::get_data(KEYS = KEYS)

    if (testData %>% is.null) next
    # Run to generate the fixtures (only the first 10 for now!)
    analysis <- KEYS %>% footballstats::generate_predictions(
      fixtureList = testData
    )

    totalAnalysed %<>% `+`(analysis$analysed)
    totalCorrect %<>% `+`(analysis$correct)
  }

  # Bind on the weekly percentages
  if (totalAnalysed > 0) {
    avgPer <- totalCorrect %>% `/`(totalAnalysed) %>% `*`(100) %>% round(2)
    weekByWeek %<>% rbind(
      data.frame(
        week = i,
        per = avgPer,
        stringsAsFactors = FALSE
      )
    )
  }

  # Print weekly percentage
  print(paste0(' ## Week [ ', i, ' ] Percentage :: ', avgPer))
}


g <- ggplot2::ggplot(
  data = weekByWeek,
  ggplot2::aes(
    x = week,
    y = per
  )) %>%
  `+`(ggplot2::geom_line()) %>%
  `+`(ggplot2::scale_y_continuous(limits = c(1, 100))) %>%
  `+`(ggplot2::ggtitle(label = 'Weekly Accuracy')) %>%
  `+`(ggplot2::xlab(label = 'Week #')) %>%
  `+`(ggplot2::ylab(label = 'Percentage')) %>%
  `+`(footballstats::plot_theme())

save(g, file = '~/Desktop/football-project/footballstats/plots/weekly-accuracy.png')
