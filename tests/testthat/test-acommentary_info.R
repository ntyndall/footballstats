context("test-add_info.R")

# Reset DB
KEYS$RED$FLUSHDB()

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

  playerInfo <- "cmp:*" %>% KEYS$RED$KEYS()
  expect_equal( playerInfo %>% length, 33 )

  singleMatch <- playerInfo %>%
    purrr::flatten_chr() %>%
    strsplit(split = ':') %>%
    purrr::map(3) %>%
    purrr::flatten_chr() %>%
    unique

  expect_equal( singleMatch %>% length, 1 )
  expect_equal( singleMatch, matchID )


  checkPlayerExists <- paste0('cmp:', KEYS$COMP, ':', matchID, ':724') %>%
    KEYS$RED$HMGET(
      field = 'name'
    ) %>%
    purrr::flatten_chr()

  expect_equal( checkPlayerExists %>% tolower, 'artur boruc' )

  # Get the general match commentaries
  teamCommentaries <- "csmt_commentary:*" %>%
    KEYS$RED$KEYS()

  teamIDs <- teamCommentaries %>%
    purrr::flatten_chr() %>%
    strsplit(split = ':') %>%
    purrr::map(5) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  expect_equal( teamIDs, c(visitorteam, localteam) %>% as.integer %>% sort )

  index <- grepl(teamIDs[1], teamCommentaries) %>% which
  teamDetails <- teamCommentaries[index] %>%
    KEYS$RED$HGETALL() %>%
    footballstats::create_hash()


  cToI <- function(x) x %>% as.character %>% as.integer

  expect_equal( teamDetails$shots_total %>% cToI(), 17 )
  expect_equal( teamDetails$shots_ongoal %>% cToI(), 9 )
  expect_equal( teamDetails$fouls %>% cToI(), 14 )
  expect_equal( teamDetails$corners %>% cToI(), 10 )

})
