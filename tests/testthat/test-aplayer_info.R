context("test-aplayer_info.R")

# Reset DB
rredis::redisFlushDB()

test_that("Check the keys are as they should be by adding player data", {

  teamID <- 9002
  playerID <- 16
  rredis::redisLPush(
    key = 'analysePlayers',
    value = playerID
  )

  expect_true( 'analysePlayers' %>% rredis::redisExists() )

  KEYS %>% footballstats::aplayer_info(
    playerLength = 1
  )

  # Check redis for expected output
  teamInfo <- 'ctps_club:1204:9002:16:2017' %>% rredis::redisHGetAll()

  expect_equal( teamInfo$name %>% as.character, 'Arsenal' )
  expect_equal( teamInfo$id %>% as.integer, teamID )

  uniqueKeys <- '*' %>% rredis::redisKeys()

  expect_equal( uniqueKeys %>% length, 5 )

  uniqueComps <- uniqueKeys %>%
    strsplit(split = ':') %>%
    purrr::map(1) %>%
    purrr::flatten_chr() %>%
    strsplit(split = 'ctps_') %>%
    purrr::map(2) %>%
    purrr::flatten_chr() %>%
    unique

  expect_true( 'club_intl' %in% uniqueComps )
  expect_true( 'cups' %in% uniqueComps )
  expect_true( 'club' %in% uniqueComps )

})
