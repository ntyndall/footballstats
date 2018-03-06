context('test-classify_form_info.R')

# Reset DB
rredis::redisFlushDB()

test_that('Calculate score based on vector of forms', {

  expect_equal( 'LLL' %>% footballstats::form_to_int(), 0 )
  expect_equal( 'DDD' %>% footballstats::form_to_int(), 3 )
  expect_equal( 'WWW' %>% footballstats::form_to_int(), 6 )
  expect_equal( 'WDL' %>% footballstats::form_to_int(), 3 )
  expect_equal( 'LDW' %>% footballstats::form_to_int(), 3 )
  expect_equal( 'W' %>% footballstats::form_to_int(), 2 )
  expect_equal( 'D' %>% footballstats::form_to_int(), 1 )
  expect_equal( 'L' %>% footballstats::form_to_int(), 0 )

})


test_that("Send in a single commentary to see it is stored correctly", {

  # Get the match info coupled to the commentary
  singleMatch <- footballstats::matchData[1, ]

  # The home team as the teamID
  res <- singleMatch %>% footballstats::current_or_other(
    teamID = sing$localteam_id
  )

  expect_equal( singleMatch$localteam_score %>% as.integer, res$current )
  expect_equal( singleMatch$visitorteam_score %>% as.integer, res$other )

  # The away team as the teamID
  res <- singleMatch %>% footballstats::current_or_other(
    teamID = sing$visitorteam_id
  )

  expect_equal( singleMatch$visitorteam_score %>% as.integer, res$current )
  expect_equal( singleMatch$localteam_score %>% as.integer, res$other )

})


test_that("Does the list return a character result of 'W' / 'L' / 'D' ", {

  subFunc <- function(c, o) footballstats::match_result(scoreCurrent = c, scoreOther = o)

  expect_equal( subFunc(c = 2, o = 1), 'W' )
  expect_equal( subFunc(c = 1, o = 1), 'D' )
  expect_equal( subFunc(c = 0, o = 1), 'L' )

})


test_that("Create team form.", {

  matchData <- footballstats::matchData %>% footballstats::order_matchdata()

  # Choose a team from the ordered data set, this one is Arsenal.
  teamID <- matchData$localteam_id[2]

  formList <- matchData %>% footballstats::team_form(
    teamID = teamID
  )

  # Calculate the number of matches this team ID has played
  numMatches <- matchData[c('localteam_id', 'visitorteam_id')] %>%
    `==`(teamID) %>%
    sum

  expect_equal( numMatches, formList[[1]] %>% length )
  expect_equal( numMatches, formList[[2]] %>% length )
  expect_equal( formList[[1]] %>% paste(collapse = ''), 'WDWWLWW' )

  sortedDates <- formList[[2]] %>% sort

  expect_equal( sortedDates[1], 17418 )
  expect_equal( sortedDates[numMatches], 17467 )

})

