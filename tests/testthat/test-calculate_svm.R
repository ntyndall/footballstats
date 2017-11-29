context('test-classify_calculate_svm.R')

competitionID <- 1204
seasonStarting <- 2017
rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()

test_that('Calculate SVM from the test match data', {

  matchData <- footballstats::amatch_info(
    competitionID = competitionID,
    dateFrom = NULL, dateTo = NULL,
    updateData = FALSE,
    seasonStarting = seasonStarting,
    analysingToday = TRUE,
    KEYS = NULL,
    bypass = TRUE)

  # Need to recreate it as new dates are created
  matchData <- footballstats::recreate_matchdata(
    competitionID = competitionID,
    seasonStarting = seasonStarting,
    matchLimit = 1000)

  footballstats::acommentary_info(
    competitionID = 1204,
    matchIDs = matchData$id,
    localteam = matchData$localteam_id,
    visitorteam = matchData$visitorteam_id,
    KEYS = NULL,
    bypass = TRUE)

  # Check the keyNames from the current list of commentarys.
  commentaryKeys <- as.character(rredis::redisKeys(
    pattern = paste0('cmt_commentary:', competitionID, '*')))
  commentaryNames <- footballstats::available_commentaries(
    commentaryKeys = commentaryKeys)

  print(commentaryNames)
  print(commentaryKeys)

  totalData <- footballstats::calculate_svm(
    competitionID = competitionID,
    seasonStarting = seasonStarting,
    commentaryKeys =  commentaryKeys,
    commentaryNames = commentaryNames,
    matchData = matchData)

  print(head(totalData))
  expect_that( totalData %>% nrow(), equals(80) )
  expect_that( totalData %>% names() %>% length(), equals(11) )
  expect_that( totalData$shots_total %>% range(), equals(c(1, 30)) )
  expect_that( totalData$shots_ongoal %>% range(), equals(c(0, 14)) )
  expect_that( totalData$fouls %>% range(), equals(c(2, 18)) )
  expect_that( totalData$corners %>% range(), equals(c(0, 11)) )
  expect_that( totalData$offsides %>% range(), equals(c(0, 8)) )
  expect_that( totalData$possesiontime %>% range(), equals(c(21, 79)) )
  expect_that( totalData$yellowcards %>% range(), equals(c(0, 5)) )
  expect_that( totalData$redcards %>% range(), equals(c(0, 1)) )
  expect_that( totalData$saves %>% range(), equals(c(0, 10)) )

  uniqueResults <- totalData$res %>% unique()

  expect_that( 'L' %in% uniqueResults, is_true() )
  expect_that( 'D' %in% uniqueResults, is_true() )
  expect_that( 'W' %in% uniqueResults, is_true() )
})
