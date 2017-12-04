context('test-classify_form_info.R')

test_that('Calculate score based on vector of forms', {

  expect_that( footballstats::form_to_int(oldForms = 'LLL'), equals(0) )
  expect_that( footballstats::form_to_int(oldForms = 'DDD'), equals(3) )
  expect_that( footballstats::form_to_int(oldForms = 'WWW'), equals(6) )
  expect_that( footballstats::form_to_int(oldForms = 'WDL'), equals(3) )
  expect_that( footballstats::form_to_int(oldForms = 'LDW'), equals(3) )
  expect_that( footballstats::form_to_int(oldForms = 'W'), equals(2) )
  expect_that( footballstats::form_to_int(oldForms = 'D'), equals(1) )
  expect_that( footballstats::form_to_int(oldForms = 'L'), equals(0) )

})


test_that("Send in a single commentary to see it is stored correctly", {

  # Get the match info coupled to the commentary
  singleMatch <- footballstats::matchData[1, ]

  # The home team as the teamID
  scoreResults <- footballstats::current_or_other(
    singleMatch = singleMatch,
    teamID = singleMatch$localteam_id)

  expect_that( singleMatch$localteam_score %>% as.integer(),
               equals(scoreResults$current %>% as.integer()) )
  expect_that( singleMatch$visitorteam_score %>% as.integer(),
               equals(scoreResults$other %>% as.integer()) )

  # The away team as the teamID
  scoreResults <- footballstats::current_or_other(
    singleMatch = singleMatch,
    teamID = singleMatch$visitorteam_id)

  expect_that( singleMatch$visitorteam_score %>% as.integer(),
               equals(scoreResults$current %>% as.integer()) )
  expect_that( singleMatch$localteam_score %>% as.integer(),
               equals(scoreResults$other %>% as.integer()) )

})


test_that("Does the list return a character result of 'W' / 'L' / 'D' ", {

  subFunc <- function(c, o) footballstats::match_result(scoreCurrent = c, scoreOther = o)

  expect_that( subFunc(c = 2, o = 1), equals('W') )
  expect_that( subFunc(c = 1, o = 1), equals('D') )
  expect_that( subFunc(c = 0, o = 1), equals('L') )

})


test_that("Create team form.", {

  matchData <- footballstats::matchData %>% footballstats::order_matchdata()

  # Choose the first team from the ordered data set
  teamID <- matchData$localteam_id[2]

  formList <- footballstats::team_form(
    matchData = matchData,
    teamID = teamID)

  numMatches <- sum(matchData[matchData$localteam_id == teamID, ] %>% nrow(),
      matchData[matchData$visitorteam_id == teamID, ] %>% nrow())

  expect_that( numMatches, equals(formList[[1]] %>% length()) )
  expect_that( numMatches, equals(formList[[2]] %>% length()) )
  expect_that( formList[[1]] %>% paste(collapse = ''), equals('WDWWLWW') )

  sortedDates <- formList[[2]] %>% sort()

  expect_that( sortedDates[1], equals(17418) )
  expect_that( sortedDates[numMatches], equals(17467) )

})

