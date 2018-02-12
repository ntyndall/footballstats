library(footballstats)
library(magrittr)

#
# This is a static script - and should be built interactively
# depending on how the model should be set up
#

# Define the season when building the model
seasonStarting <- 2017

# Define a single competitionID if only one is to be used
competitionID <- 1204

# Get allowed competitions
comps <- footballstats::allowed_comps()

# Get all the data first
cat(paste0(Sys.time(), ' | Recreating match data. \n'))
totalData <- data.frame(stringsAsFactors = FALSE)
for (i in 1:(comps %>% length)) {

  competitionID <- comps[i]
  matchData <- footballstats::recreate_matchdata(
    competitionID = competitionID,
    seasonStarting = seasonStarting,
    matchLimit = 10000
  )
  totalData %<>% rbind(matchData)

  # Build league table
  matchData %>% footballstats::create_table()

  # Store positions on a weekly basis
  footballstats::weekly_positions(
    competitionID = competitionID,
    seasonStarting = seasonStarting
  )
}

# Only select one competition
totalData <- totalData[totalData$comp_id == '1204', ]

# Construct data set for building a classifier (for some reason this is very slow.. and can hang)
cat(paste0(Sys.time(), ' | Creating a dataframe from the match data. \n'))
groups <- 100
loops <- totalData %>% nrow %>% `/`(groups) %>% ceiling
original.data <- data.frame(stringsAsFactors = FALSE)
for (i in 1:loops) {
  upper <- if (i %>% `==`(loops)) {
    totalData %>% nrow %>% mod(groups) %>% `+`(groups %>% `*`(loops - 1))
  } else {
    groups %>% `*`(i)
  }
  lower <- groups %>% `*`(i - 1) %>% `+`(1)
  original.data %<>% rbind(totalData[lower:upper, ] %>% footballstats::calculate_data())
}
# Only look at complete rows!
original.data %<>% subset(original.data %>% stats::complete.cases())

# Drop the match IDs
original.data <- original.data[ , 2:(original.data %>% ncol)]

# Create scaled data set
dataScales <- original.data %>% footballstats::get_scales()
commentaries <-  c(
  'shots_total', 'shots_ongoal', 'fouls', 'corners', 'possesiontime', 'yellowcards', 'saves'
)
dataScales$commentaries <- c(commentaries %>% paste0('.h'), commentaries %>% paste0('.a'))
save(dataScales, file = getwd() %>% paste0('/data/dataScales.rda'))

# Scale the original data set
original.data %<>% footballstats::scale_data(dataScales = dataScales)

# Build the neural network with scaled data
cat(paste0(Sys.time(), ' | Building Neural Network. \n'))
nn <- original.data %>% footballstats::neural_network()



save(nn, file = getwd() %>% paste0('/data/nn.rda'))
