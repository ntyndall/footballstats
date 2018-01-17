library(footballstats)
library(magrittr)

# Get allowed competitions
comps <- footballstats::allowed_comps()

# Get all the data first
cat(paste0(Sys.time(), ' | Recreating match data. \n'))
totalData <- data.frame(stringsAsFactors = FALSE)
for (i in 1:(comps %>% length)) {
  matchData <- footballstats::recreate_matchdata(
    competitionID = comps[i],
    seasonStarting = seasonStarting,
    matchLimit = 10000)
  totalData %<>% rbind(matchData)
}

# Construct data set for building an SVM
cat(paste0(Sys.time(), ' | Creating a dataframe from the match data. \n'))
original.data <- totalData %>% footballstats::calculate_data()

# Replace any NA's with zero
original.data[original.data %>% is.na] <- 0

# Create scaled data set
dataScales <- original.data %>% footballstats::get_scales()
save(dataScales, file = getwd() %>% paste0('/data/dataScales.rda'))

# Scale the original data set
original.data %<>% footballstats::scale_data(dataScales = dataScales)

# Build the neural network with scaled data
cat(paste0(Sys.time(), ' | Building Neural Network. \n'))
classifyModel <- original.data %>% footballstats::neural_network()
save(classifyModel, file = getwd() %>% paste0('/data/classifyModel.rda'))

