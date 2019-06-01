context("test-aevent_info.R")

# Reset DB
KEYS$RED$FLUSHDB()

test_that("Can a single event be added to redis", {

  oneMatchEvents <- footballstats::matchData$events[1]

  # Run function
  KEYS %>% footballstats::aevent_info(
    matchIDs = footballstats::matchData$id[1],
    matchEvents = oneMatchEvents
  )

  # Get all event ID's from set
  eventsInRedis <- "c_eventInSet:1204:2017" %>%
    KEYS$RED$SMEMBERS() %>%
    purrr::flatten_chr()

  # All the ID's are added
  expect_true( eventsInRedis %in% oneMatchEvents[[1]]$id %>% all )

  # All the HASH maps are added correctly
  expect_equal( "*" %>% KEYS$RED$KEYS() %>% length, eventsInRedis %>% length %>% `+`(1) )

  # Running this again has no effect
  KEYS %>% footballstats::aevent_info(
    matchIDs = footballstats::matchData$id[1],
    matchEvents = oneMatchEvents
  )

  # And the tests dont change either
  expect_true( eventsInRedis %in% oneMatchEvents[[1]]$id %>% all )
  expect_equal( "*" %>% KEYS$RED$KEYS() %>% length, eventsInRedis %>% length %>% `+`(1) )

  KEYS$RED$FLUSHDB()

})


test_that("Adding multiple events to redis", {

  allEvents <- footballstats::matchData$events

  # Run function
  KEYS %>% footballstats::aevent_info(
    matchIDs = footballstats::matchData$id,
    matchEvents = allEvents
  )

  # Get total number of events
  numRows <- allEvents %>%
    purrr::map(nrow) %>%
    purrr::flatten_int() %>%
    sum

  # Get all event ID's from set
  eventsInRedis <- "c_eventInSet:1204:2017" %>%
    KEYS$RED$SMEMBERS() %>%
    purrr::flatten_chr()

  # Check all have been added
  expect_equal( "*" %>% KEYS$RED$KEYS() %>% length, numRows %>% `+`(1) )
  expect_equal( eventsInRedis %>% length, numRows )

  # Running it again has no effect
  KEYS %>%
    footballstats::aevent_info(
    matchIDs = footballstats::matchData$id,
    matchEvents = allEvents
  )

  # Check all have been added previously and are unchanged
  expect_equal( "*" %>% KEYS$RED$KEYS() %>% length, numRows %>% `+`(1) )
  expect_equal( eventsInRedis %>% length, numRows )

  # Check a single hash
  singleEvent <- "cme:1204:2212979:234196612" %>%
    KEYS$RED$HGETALL() %>%
    footballstats::create_hash()

  # Check stats are correct
  expect_equal( singleEvent$minute %>% as.integer, 87 )
  expect_equal( singleEvent$player_id %>% as.integer, 175302 )

  KEYS$RED$FLUSHDB()

})
