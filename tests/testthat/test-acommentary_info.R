context("test-add_info.R")

# Reset DB
rredis::redisFlushDB()

test_that("Send in a single commentary to see it is stored correctly", {

  # Get the match info coupled to the commentary
  commentaryData <- footballstats::commentaryData[[1]]
  matches <- footballstats::matchData
  localteam <- matches$localteam_id[1]
  visitorteam <- matches$visitorteam_id[1]
  matchID <- commentaryData$match_id

  footballstats::acommentary_info(
    competitionID = competitionID,
    matchIDs = matchID,
    localteam = localteam,
    visitorteam = visitorteam,
    KEYS = KEYS)

  playerInfo <- 'cmp:*' %>% rredis::redisKeys()
  expect_that( playerInfo %>% length, equals(33) )

  singleMatch <- playerInfo %>%
    strsplit(split = ':') %>%
    purrr::map(3) %>%
    purrr::flatten_chr() %>%
    unique

  expect_that( singleMatch %>% length, equals(1) )

  checkPlayerExists <- rredis::redisHMGet(
    key = paste0('cmp:', competitionID, ':', matchID, ':153493'),
    fields = 'name')

  expect_that( checkPlayerExists$name %>% as.character %>% tolower, equals('mohamed elneny') )

  # Get the general match commentaries
  teamCommentaries <- rredis::redisKeys(
    pattern = 'cmt_commentary:*')

  teamIDs <- teamCommentaries %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  expect_that( teamIDs, equals(c(visitorteam, localteam) %>% as.integer %>% sort) )

  index <- which(grepl(teamIDs[1], teamCommentaries))
  teamDetails <- rredis::redisHGetAll(
    key = teamCommentaries[index])

  expect_that( teamDetails$shots_total %>% as.character %>% as.integer, equals(25) )
  expect_that( teamDetails$shots_ongoal %>% as.character %>% as.integer, equals(12) )
  expect_that( teamDetails$fouls %>% as.character %>% as.integer, equals(7) )
  expect_that( teamDetails$corners %>% as.character %>% as.integer, equals(6) )

})
