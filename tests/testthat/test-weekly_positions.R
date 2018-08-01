context('test-weekly_positions.R')

# Flush db
KEYS$RED$FLUSHDB()

test_that("Create a table from match data", {

  # Order the match data first and create a table
  matchData <- footballstats::matchData %>%
    footballstats::order_matchdata()

  KEYS %>% footballstats::create_table(
    matchData = matchData
  )

  KEYS %>% footballstats::weekly_positions()

  # Creates from 1 - Team in leage hash of positions of form cw_pl
  check_weekly <- function(KEYS) {
    allKeys <- "cw_pl:*" %>%
      KEYS$RED$KEYS() %>%
      purrr::flatten_chr()

    expect_equal( allKeys %>% length, 7 )

    # Order the keys
    allKeys %<>% `[`(
      allKeys %>%
        strsplit(split = ':') %>%
        purrr::map(4) %>%
        purrr::flatten_chr() %>%
        as.integer %>%
        order
    )

    # Check west ham (team id = 9427)
    positions <- KEYS$RED$pipeline(
      .commands = lapply(
        X = allKeys,
        FUN = function(x) x %>% KEYS$PIPE$HGET("9427")
      )
    ) %>%
      purrr::flatten_chr() %>%
      as.integer

    expect_equal( positions, c(5, 6, 10, 7, 9, 10, 11) )
  }

  # Check results
  KEYS %>% check_weekly()


  KEYS$RED$FLUSHDB()

})


