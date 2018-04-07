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
  expect_equal( singleMatch, matchID )

  checkPlayerExists <- rredis::redisHMGet(
    key = paste0('cmp:', KEYS$COMP, ':', matchID, ':724'),
    fields = 'name'
  )

  expect_equal( checkPlayerExists$name %>% as.character %>% tolower, 'artur boruc' )

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

  cToI <- function(x) x %>% as.character %>% as.integer

  expect_equal( teamDetails$shots_total %>% cToI(), 17 )
  expect_equal( teamDetails$shots_ongoal %>% cToI(), 9 )
  expect_equal( teamDetails$fouls %>% cToI(), 14 )
  expect_equal( teamDetails$corners %>% cToI(), 10 )

})
