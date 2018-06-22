library(footballstats)
library(magrittr)

# This is a static script - and should be built interactively
# depending on how the model should be set up
#

# Define the season when building the model
seasonStarting <- 2017

# Define a single competitionID if only one is to be used
#competitionID <- 1204

# Get allowed competitions
comps <- footballstats::allowed_comps()

# Start up redis
footballstats::redis_con()

# Get all the data first
cat(paste0(Sys.time(), ' | Recreating match data. \n'))
totalData <- data.frame(stringsAsFactors = FALSE)
for (i in 1:(comps %>% length)) {

  print(paste0('Comp ', i, ' / ', comps %>% length))
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
  print(paste0(' Looping ', i, ' / ', loops))

  # Get the upper index
  upper <- if (i %>% `==`(loops)) {
    totalData %>%
      nrow %>%
      magrittr::mod(groups) %>%
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

# big.original.data <- original.data
# big.original.data -> original.data

# Only look at complete rows!
original.data %<>% subset(original.data %>% stats::complete.cases())

# Drop the match IDs
original.data <- original.data[ , 2:(original.data %>% ncol)]


#
#

# Create scaled data set
dataScales <- original.data %>% footballstats::get_scales()
commentaries <-   c('shots_total', 'shots_ongoal', 'possesiontime')
dataScales$commentaries <- c(commentaries %>% paste0('.h'), commentaries %>% paste0('.a'))

# Save the scaled data set
save(dataScales, file = getwd() %>% paste0('/data/dataScales.rda'))

# Scale the original data set
original.data %<>% footballstats::scale_data(dataScales = dataScales)

# Build the neural network with scaled data
cat(paste0(Sys.time(), ' | Building Neural Network. \n'))
#
#
#


#
#
my.original.data -> original.data
eliminate <- c('fouls', 'corners', 'yellowcards')

new.data <- subset(original.data, select = -c(paste0(eliminate, '.h'), paste0(eliminate, '.a')))
original.data[ , -which(names(original.data) %in% c(paste0(eliminate, '.h'), paste0(eliminate, '.a')))] -> new.data


bestResult <- 0
allResults <- meanRes <- c()
ITER <- 20
THRESH <- 0.005

# splitting <- original.data$res %>% caTools::sample.split(SplitRatio = 0.70)
for (j in 1:ITER) {
  if (j == 1) tStart <- Sys.time()
  #splitting <- original.data$res %>% caTools::sample.split(SplitRatio = 0.70)
  nn <- original.data %>% neural_network(
    NN = list(REP = 1, THRESH = THRESH)
  )

  # Save all results
  allResults %<>% c(nn$result)

  # Save the new best NN
  if (nn$result > bestResult) {
    bestNN <- nn$neural
    bestResult <- nn$result
  }

  if (j == ITER) {
    tEnd <- Sys.time()
    meanRes %<>% c(allResults %>% list)
  }
}
# Total time spent
difftime(tEnd, tStart, units = 'secs') %>%
  as.integer %>%
  cat

#
#
#
#
#
#
#

table(allNames)
allNames %>% unique

# Check ALL the data!!
check.data <- original.data
realVec <- original.data$res %>% as.character
newLabels <- original.data$res %>% unique %>% sort %>% as.character
check.data$res <- NULL

predictions <- neuralnet::compute(
  x = nn,
  covariate = check.data
)

# Create vectors to measure accuracy
predVec <- 0 %>% rep(check.data %>% nrow)
tot <- 0

# Check the max values per row for the predictions
netRes <- predictions$net.result
for (j in 1:(netRes %>% nrow)) predVec[j] <- newLabels[netRes[j, ] %>% which.max]

Actual.score <- realVec %>% factor(levels = newLabels)
Predicted.score <- predVec %>% factor(levels = newLabels)

# Build a table of results
resultTable <- table(Actual.score, Predicted.score)
print(' ## Confusion matrix ##')
print(caret::confusionMatrix(data = resultTable))

# Save the neural network
save(nn, file = getwd() %>% paste0('/data/nn.rda'))
