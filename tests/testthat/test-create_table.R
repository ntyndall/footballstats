context('test-create_table.R')

# Flush db
KEYS$RED$FLUSHDB()

test_that("Create a table from match data", {

  # Order the match data first
  matchData <- footballstats::matchData %>%
    footballstats::rename_columns(mapping = "api") %>%
    footballstats::order_matchdata()

  KEYS %>% footballstats::create_table(
    matchData = matchData
  )

  # Function to make sure remains the same
  check_table <- function(KEYS) {
    # Follow a teamID
    teamID <- 9427
    allKeys <- paste0("*:", teamID) %>%
      KEYS$RED$KEYS() %>%
      purrr::flatten_chr()

    # Check basic key lengths
    expect_equal( "*" %>% KEYS$RED$KEYS() %>% length, 138 )
    expect_equal( "leagueMatchSet:2017" %>% KEYS$RED$SMEMBERS() %>% length, 140 )

    # Create function for mapping and flattening
    mf <- function(x, y) x %>% purrr::map(y) %>% purrr::flatten_chr()

    # Order the keys
    allKeys %<>% `[`(
      allKeys %>%
        strsplit(split = ':') %>%
        mf(4) %>%
        as.integer %>%
        order
    )

    results <- KEYS$RED$pipeline(
      .commands = lapply(
        X = allKeys,
        FUN  = function(x) x %>% KEYS$PIPE$HGETALL()
      )
    ) %>%
      lapply(footballstats::create_hash)

    mf <- function(x, y) x %>% purrr::map(y) %>% purrr::flatten_chr()

    expect_equal( results %>% mf(1) %>% unique %>% tolower, "west ham united") # Team name
    expect_equal( results %>% mf(2) %>% as.integer %>% sum, 35 ) # Points
    expect_equal( results %>% mf(3) %>% as.integer %>% sum, 27 ) # GF
    expect_equal( results %>% mf(4) %>% as.integer %>% sum, 5 ) # GD
  }

  # Check results
  KEYS %>% check_table()

  # Run again
  KEYS %>% footballstats::create_table(
    matchData = matchData
  )

  # Check results haven't changed
  KEYS %>% check_table()
  KEYS$RED$FLUSHDB()

  # Make sure I can overlap the match data and it still works the same
  loI <- c(1, 3, 34, 39)
  upI <- c(2, 40, 40, 70)
  for (i in 1:(loI %>% length)) {
    KEYS %>% footballstats::create_table(
      matchData = matchData[loI[i]:upI[i], ]
    )
  }

  KEYS %>% check_table()
  KEYS$RED$FLUSHDB()

})
