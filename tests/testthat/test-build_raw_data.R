
KEYS$RED$FLUSHDB()

test_that("Can I build a simple raw data frame before it gets compressed to aggregate statistics", {

  # Insert the necessary keys for a single fixture
  KEYS %>% footballstats::insert_headtohead()

  # Recreate the match data that is in redis
  inter.data <- KEYS %>%
    build_raw_data(
      singleFixture = footballstats::headtohead$single %>% footballstats::rename_columns(mapping = "api")
    )

  expect_is( inter.data, "data.frame" )
  expect_equal( inter.data %>% nrow, 12 )
  expect_equal( inter.data$til %>% unique, KEYS$TIL )

  # Every third row is a new team
  for (i in 1:4) {
    type <- if (i %% 2 %>% `==`(0)) "awayID" else "localID"
    expect_equal( inter.data[[type]][(1 + (i - 1)*3):(i*3)] %>% unique %>% length, 1 )
  }

  # Check some arbitrary sums sums
  expect_equal( inter.data$position.h %>% sum, 105 )
  expect_equal( inter.data$position.a %>% sum, 131 )
  expect_equal( inter.data$shots_total.h %>% as.double %>% sum, 174 )
  expect_equal( inter.data$shots_total.a %>% as.double %>% sum, 111 )

})
