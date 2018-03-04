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

  KEYS %>% footballstats::acommentary_info(
    matchIDs = matchID,
    localteam = localteam,
    visitorteam = visitorteam
  )

  playerInfo <- 'cmp:*' %>% rredis::redisKeys()
  expect_equal( playerInfo %>% length, 33 )

  singleMatch <- playerInfo %>%
    strsplit(split = ':') %>%
    purrr::map(3) %>%
    purrr::flatten_chr() %>%
    unique

  expect_equal( singleMatch %>% length, 1 )

  checkPlayerExists <- rredis::redisHMGet(
    key = paste0('cmp:', KEYS$COMP, ':', matchID, ':153493'),
    fields = 'name'
  )

  expect_equal( checkPlayerExists$name %>% as.character %>% tolower, 'mohamed elneny' )

  # Get the general match commentaries
  teamCommentaries <- 'cmt_commentary:*' %>% rredis::redisKeys()

  teamIDs <- teamCommentaries %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  expect_equal( teamIDs, c(visitorteam, localteam) %>% as.integer %>% sort )

  index <- grepl(teamIDs[1], teamCommentaries) %>% which
  teamDetails <-  teamCommentaries[index] %>% rredis::redisHGetAll()

  expect_equal( teamDetails$shots_total %>% as.character %>% as.integer, 25 )
  expect_equal( teamDetails$shots_ongoal %>% as.character %>% as.integer, 12 )
  expect_equal( teamDetails$fouls %>% as.character %>% as.integer, 7 )
  expect_equal( teamDetails$corners %>% as.character %>% as.integer, 6 )

})
