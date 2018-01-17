#' @title classify_all
#'
#' @description A function that loops through all possibilities of the variables
#'  to be included in the classifier. That means the recent form and results
#'  will be better represented as the best classifier will be built on the most
#'  recent results.
#'
#' @details 1) classify_recreate_matchdata():
#'   2) calculateSVMData() -> classify_match_result() -> { classify_team_form() -> classify_match_result() }
#'      -> calculateAdditionalMetrics():
#'   3) getBinns():
#'   4) optimizeSVM() -> calculateBestSVMFit()
#'      -> { classify_generate_predictions() ->
#'                  classify_emoji() ->
#'                  { classify_homeaway_stats() -> classify_commentary_stats() } ->
#'                  { classify_form_from_match() -> classify_match_result() } ->
#'                  mapFormToInteger() }
#'   5) predictFutureMatches() -> formatDates() -> getGeneralData() ->  { classify_generate_predictions() -> ... }
#'
#' @param competitionID
#' @param seasonStarting
#' @param returnItems
#' @param matchLimit
#'
#' @return A list containing the best SVM calculated in the first key, and
#'  the best factors to use in the second key.
#'
#' @export



classify_all <- function(competitionID, competitionName, seasonStarting,
                         returnItems, printToSlack = FALSE, KEYS) {

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

  # Predict actual future results
  cat(paste0(Sys.time(), ' | Predicting actual upcoming fixtures. \n'))
  footballstats::predict_matches(
    competitionID = competitionID,
    competitionName = competitionName,
    dataScales = dataScales,
    classifyModel = classifyModel,
    KEYS = KEYS)
}
