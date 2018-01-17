library(footballstats)

# Query Redis and return everything from the competition.
cat(paste0(Sys.time(), ' | Recreating match data. \n'))
matchData <- footballstats::recreate_matchdata(
  competitionID = competitionID,
  seasonStarting = seasonStarting,
  matchLimit = matchLimit)

# Check the keyNames from the current list of commentarys.
commentaryNames <- competitionID %>% footballstats::available_commentaries(
  includeNames = returnItems)

# Construct data set for building an SVM
cat(paste0(Sys.time(), ' | Creating a dataframe from the match data. \n'))
totalData <- footballstats::calculate_svm(
  commentaryNames = commentaryNames,
  matchData = matchData)

# Create scaled data set
dataScales <- totalData %>% footballstats::get_scales()

totalData %<>% footballstats::scale_data(
  dataScales = dataScales)

# Optimize the SVM by looping through all available variables
# cat(paste0(Sys.time(), ' | Optimizing the SVM Classifier. \n'))
#SVMDetails <- totalData %>%
#  footballstats::optimize_svm()

cat(paste0(Sys.time(), ' | Building Neural Network. \n'))
classifyModel <- totalData %>% footballstats::neural_network()

# Save the model somewhere??
