library(footballstats)
library(magrittr)

# This is a static script - and should be built interactively
# depending on how the model should be set up
#

# Define the season when building the model
#seasonStarting <- 2017

# Define a single competitionID if only one is to be used
#competitionID <- 1204

# Get allowed competitions
comps <- footballstats::allowed_comps()

# Get all the data first
cat(paste0(Sys.time(), ' | Recreating match data. \n'))
totalData <- data.frame(stringsAsFactors = FALSE)
for (i in 1:(comps %>% length)) {

  # Define the keys for each recreation
  KEYS$COMP <- comps[i]
  KEYS$SEASON <- seasonStarting

  # Recreate the match data
  matchData <- KEYS %>% footballstats::recreate_matchdata()
  totalData %<>% rbind(matchData)

  # Build league table
  matchData %>% footballstats::create_table()

  # Store positions on a weekly basis
  KEYS %>% footballstats::weekly_positions()
}

# Remove any matches that can't be found by the API (i.e. no FT score)!
totalData %<>% subset(totalData$ft_score %>% `!=`('[-]'))

# Construct data set for building a classifier (for some reason this is very slow.. and can hang)
cat(paste0(Sys.time(), ' | Creating a dataframe from the match data. \n'))
groups <- 100

# Get the total number of iterations
loops <- totalData %>%
  nrow %>%
  `/`(groups) %>%
  ceiling

original.data <- data.frame(stringsAsFactors = FALSE)
for (i in 1:loops) {
  cat(' Looping ', i, ' / ', loops)

  # Get the upper index
  upper <- if (i %>% `==`(loops)) {
    totalData %>%
      nrow %>%
      mod(groups) %>%
      `+`(groups %>% `*`(loops - 1))
  } else {
    groups %>% `*`(i)
  }

  # Get the lower index
  lower <- groups %>%
    `*`(i - 1) %>%
    `+`(1)

  # Bind the data set on each time
  original.data %<>% rbind(totalData[lower:upper, ] %>% footballstats::calculate_data())
}

# Only look at complete rows!
original.data %<>% subset(original.data %>% stats::complete.cases())

# Drop the match IDs
original.data <- original.data[ , 2:(original.data %>% ncol)]

# Create scaled data set
dataScales <- original.data %>% footballstats::get_scales()
commentaries <-  c(
  'shots_total', 'shots_ongoal', 'fouls', 'corners',
  'possesiontime', 'yellowcards', 'saves'
)
dataScales$commentaries <- c(commentaries %>% paste0('.h'), commentaries %>% paste0('.a'))

# Save the scaled data set
save(dataScales, file = getwd() %>% paste0('/data/dataScales.rda'))

# Scale the original data set
original.data %<>% footballstats::scale_data(dataScales = dataScales)

# Build the neural network with scaled data
cat(paste0(Sys.time(), ' | Building Neural Network. \n'))
nn <- original.data %>% footballstats::neural_network()

# Save the neural network
save(nn, file = getwd() %>% paste0('/data/nn.rda'))
