
# Load the data set
KEYS <- footballstats::sensitive_keys(
  printToSlack = TRUE,
  printToScreen = FALSE,
  testing = FALSE,
  storePred = TRUE
)

# Load competitions and run the functionality below.
competitions <- KEYS %>% footballstats::acomp_info()

# Subset the available competitions
competitions %<>% subset(competitions$id %in% footballstats::allowed_comps())

KEYS$COMP <- "1205"
KEYS$TIL <- KEYS$COMP %>% footballstats::teams_in_league()
KEYS$SEASON <- 2018
KEYS$DATE_FROM <- Sys.Date() %>% `-`(1) %>% footballstats::format_dates()
KEYS$DATE_TO <- Sys.Date() %>% `+`(6) %>% footballstats::format_dates()
KEYS$TEST <- TRUE
KEYS$SLACK_PRNT <- FALSE
KEYS$LOG_PRED <- FALSE

# Get fixtures
fixtureList <-
  paste0("/matches?comp_id=", KEYS$COMP, "&from_date=", KEYS$DATE_FROM, "&to_date=", KEYS$DATE_TO, "&") %>%
    footballstats::get_data(KEYS = KEYS)


# Define default GRIDS list
GRIDS <- list(
  DAYS = 3,
  GRID_PTS = 10,
  GRID_BOUND = 0.2,
  DECAY = 5000,
  TOTAL_PERC = 0.5,
  NN_REP = 1,
  NN_THRESH = 0.005,
  XG_ROUNDS = 100000,
  XG_DEPTH = 2,
  XG_ETA = 0.1,
  XG_GAMMA = 1,
  XG_BOUNDARY = 6
)

xg_depth <- c(2, 4, 6, 8)
xg_eta <- c(0.1, 0.2, 0.3, 0.4)
xg_gamma <- c(0.1, 0.5, 1, 2, 8)
pts <- c(8, 10, 12, 15)
myper <- c(0.5)
boundary <- c(14, 12, 10, 8)

# Loop over XG parameters
for (i in pts) {
  for (j in xg_eta[1]) {
    for (k in boundary) {
      for (l in myper) {
        # Reassign grid values
        KEYS$PARAM_TOTALPER <- GRIDS$TOTAL_PERC
        KEYS$PARAM_GPOINTS <- GRIDS$GRID_PTS
        KEYS$XG_BOUND <- GRIDS$XG_BOUNDARY

        totalPredictions <- 0
        # Call the function
        myres <- footballstats::total.metrics %>%
          footballstats::optimize_variables(
            optimizeModels = FALSE,
            GRIDS = GRIDS,
            types = "xgboost"
          )

        cat(" ## TOTAL PER :", l, " ||  POINTS :", i, " ||  BOUNDARY :", k, "\n")
        cat(" ## Accuracy :", myres$xgb$totalStats$totAcc %>% mean, "/", myres$xgb$totalStats$totAcc %>% stats::sd(), "\n")

        predictions <- KEYS %>%
          footballstats::predict_matches(
            datModel = myres$xgb$model,
            cMethod = "xgboost",
            fixtureList
          )
      }
    }
  }
}


# save the model
xgModel <- myres$xgb$model
save(xgModel, file = "~/Documents/footballstats/xgModel.rda")
