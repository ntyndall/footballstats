#' @title Predictions vs Actual Results
#' @export


predict_vs_real <- function(competitionID, readyToAnalyseKey, matches) {

  matchIDs <- rredis::redisSMembers(
    set = readyToAnalyseKey
  ) %>% purrr::flatten_chr()

  readyToAnalyse <- intersect(matches$id, matchIDs)
  if (!identical(readyToAnalyse, character(0))) {
    print(paste0(' Checking off ', length(readyToAnalyse), ' already predicted matches.'))

    for (i in 1:length(readyToAnalyse)) {
      matchID <- readyToAnalyse[i]
      resultKey <- paste0('c:', competitionID, ':pred:', matchID)
      predicted <- rredis::redisHGetAll(
        key = resultKey)

      if (!('prediction' %in% (predicted %>% names()))) {

        # Convert data to something meaningful
        home <- predicted$home %>% as.character()
        away <- predicted$away %>% as.character()

        actual <- matches[which(matches$id == matchID), ]
        hm <- actual$localteam_score %>% as.integer()
        aw <- actual$visitorteam_score %>% as.integer()

        cond <- function(h, a) {
         return(ifelse(
           test = home == h && away == a,
           yes = TRUE,
           no = FALSE))
        }

        # Was the prediction correct?
        if (hm > aw) {
          correct <- cond(h = 'W', a = 'L')
        } else if (hm == aw) {
          correct <- cond(h = 'D', a = 'D')
        } else {
          correct <- cond(h = 'L', a = 'W')
        }

        # Update hash with the results of the prediction
        rredis::redisHSet(
          key = resultKey,
          field = 'prediction',
          value =  ifelse(test = correct, yes = 'T', no = 'F') %>% charToRaw())
      }
    }
  }
}
