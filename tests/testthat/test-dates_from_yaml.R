context("test-dates_from_yaml")


test_that("Create head to head data set", {

  # Time in 2017
  tNow <- "10/11/2017" %>%
    as.Date(format = "%d/%m/%Y")

  # Initialise the season
  newKeys <- KEYS %>%
    footballstats::dates_from_yaml(
      tNow = tNow
    )

  # Should be set as 2017
  expect_equal( "c_currentSeason:1204" %>% KEYS$RED$GET(), tNow %>% format("%Y") )
  expect_false( newKeys$ACTIVE )

  # Now try again in 2018
  tNow <- "01/02/2018" %>%
    as.Date(format = "%d/%m/%Y")

  # ... try again
  newKeys <- KEYS %>%
    footballstats::dates_from_yaml(
      tNow = tNow
    )

  # Should be set as 2017
  expect_equal( "c_currentSeason:1204" %>% KEYS$RED$GET(), tNow %>% format("%Y") )
  expect_false( newKeys$ACTIVE )

  # Finally, pick a date within the league start and end time
  tNow <- "01/10/2018" %>%
    as.Date(format = "%d/%m/%Y")

  # Active date
  newKeys <- KEYS %>%
    footballstats::dates_from_yaml(
      tNow = tNow
    )

  # Season should be active now
  expect_equal( "c_currentSeason:1204" %>% KEYS$RED$GET(), tNow %>% format("%Y") )
  expect_equal( newKeys$SEASON, 2018 )
  expect_true( newKeys$ACTIVE )

})
