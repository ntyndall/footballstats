context("test-ateam_info.R")

rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()

test_that("Check the keys are as they should be by adding team data", {

  teamID <- 9002
  rredis::redisLPush(
    key = 'analyseTeams',
    value = teamID)

  expect_that( 'analyseTeams' %>% rredis::redisExists(), is_true() )

  footballstats::ateam_info(
    competitionID = 1204,
    teamListLength = 1,
    KEYS = KEYS)

  # Check redis for expected output
  teamIDs <- 'ctp:*' %>%
    rredis::redisKeys() %>%
    strsplit(split = ':') %>%
    purrr::map(4) %>%
    purrr::flatten_chr() %>%
    as.integer %>%
    sort

  # Check the keys exist
  expect_that( 'ct_basic:1204:9002' %>% rredis::redisExists(), is_true() )
  expect_that( 'ct_stats:1204:9002' %>% rredis::redisExists(), is_true() )
  ## expect_that( 'analyseTeams' %>% rredis::redisExists(), is_false() )

  # Check basic statistics
  basicStats <- 'ct_basic:1204:9002' %>% rredis::redisHGetAll()

  expect_that( basicStats %>% names %>% length, equals(14) )
  expect_that( basicStats$team_id %>% as.integer, equals(teamID) )
  expect_that( basicStats$name %>% as.character, equals('Arsenal') )

  # Check general statistics
  teamStats <- 'ct_stats:1204:9002' %>% rredis::redisHGetAll()

  expect_that( teamStats %>% names %>% length, equals(49) )
  expect_that( teamStats$wins %>% as.integer, equals(6) )

})
