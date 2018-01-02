context("test-aplayer_info.R")

rredis::redisConnect(
  host = 'localhost',
  port = 6379)
rredis::redisSelect(3)
rredis::redisFlushDB()

test_that("Check the keys are as they should be by adding player data", {

  teamID <- 9002
  playerID <- 16
  rredis::redisLPush(
    key = 'analysePlayers',
    value = playerID)

  expect_that( 'analysePlayers' %>% rredis::redisExists(), is_true() )

  footballstats::aplayer_info(
    competitionID = 1204,
    playerLength = 1,
    currentSeasonYear = 2017,
    KEYS = NULL,
    bypass = TRUE)

  # Check redis for expected output
  teamInfo <- 'ctps_club:1204:9002:16:2017' %>% rredis::redisHGetAll()

  expect_that( teamInfo$name %>% as.character, equals('Arsenal') )
  expect_that( teamInfo$id %>% as.integer,  equals(teamID) )

  uniqueKeys <- '*' %>% rredis::redisKeys()

  expect_that( uniqueKeys %>% length, equals(5) )

  uniqueComps <- uniqueKeys %>%
    strsplit(split = ':') %>%
    purrr::map(1) %>%
    purrr::flatten_chr() %>%
    strsplit(split = 'ctps_') %>%
    purrr::map(2) %>%
    purrr::flatten_chr() %>%
    unique

  expect_that( 'club_intl' %in% uniqueComps, is_true() )
  expect_that( 'cups' %in% uniqueComps, is_true() )
  expect_that( 'club' %in% uniqueComps, is_true() )

})
