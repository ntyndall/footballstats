context("test-ateam_info.R")

# Reset DB
KEYS$RED$FLUSHDB()

test_that("Check the keys are as they should be by adding team data", {

  teamID <- 9002
  "analyseTeams" %>% KEYS$RED$LPUSH(
    value = teamID
  )

  expect_true( 'analyseTeams' %>% KEYS$RED$EXISTS() %>% as.logical )

  KEYS %>% footballstats::ateam_info(
    teamListLength = 1
  )

  # Check redis for expected output
  teamIDs <- 'ctp:*' %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr() %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  # Check the keys exist
  expect_true( 'ct_basic:1204:9002' %>% KEYS$RED$EXISTS() %>% as.logical )
  expect_true( 'ct_stats:1204:9002' %>%KEYS$RED$EXISTS() %>% as.logical )
  expect_false( 'analyseTeams' %>% KEYS$RED$EXISTS() %>% as.logical )

  # Check basic statistics
  basicStats <- 'ct_basic:1204:9002' %>%
    KEYS$RED$HGETALL() %>%
    footballstats::create_hash()

  expect_equal( basicStats %>% names %>% length, 14 )
  expect_equal( basicStats$team_id %>% as.integer, teamID )
  expect_equal( basicStats$name %>% as.character, 'Arsenal' )

  # Check general statistics
  teamStats <- 'ct_stats:1204:9002' %>%
    KEYS$RED$HGETALL() %>%
    footballstats::create_hash()

  expect_equal( teamStats %>% names %>% length, 49 )
  expect_equal( teamStats$wins %>% as.integer, 6 )

})
