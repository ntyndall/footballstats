#' @title Predictions vs Actual Results
#' @export


predict_vs_real <- function(competitionID, readyToAnalyse, matches) {

  readyToAnalyse <- readyToAnalyse %>%
    strsplit(split = '[:]') %>%
    purrr::map(4) %>%
    purrr::flatten_chr()

  readyToAnalyse <- intersect(matches$id, matchIDs)
  if (!identical(readyToAnalyse, character(0))) {
    readyLen <- readyToAnalyse %>% length
    print(paste0(' Checking off ', readyLen, ' already predicted matches.'))

    for (i in 1:readyLen) {
      matchID <- readyToAnalyse[i]
      resultKey <- paste0('c:', competitionID, ':pred:', matchID)
      predicted <- resultKey %>% rredis::redisHGetAll()

      if (!('prediction' %in% (predicted %>% names))) {

        # Convert data to something meaningful
        home <- predicted$home %>% as.character
        away <- predicted$away %>% as.character

        actual <- matches[which(matches$id == matchID), ]
        hm <- actual$localteam_score %>% as.integer
        aw <- actual$visitorteam_score %>% as.integer

        cond <- function(h, a) {
         return(ifelse(
           test = home == h && away == a,
           yes = TRUE,
           no = FALSE))
        }

        # Was the prediction correct?
        correct <- if (hm > aw) {
          cond(h = 'W', a = 'L')
        } else if (hm == aw) {
          cond(h = 'D', a = 'D')
        } else {
          cond(h = 'L', a = 'W')
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
