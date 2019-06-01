context("test-missing_commentaries")

# Reset DB
KEYS$RED$FLUSHDB()


test_that("Make sure if commentary doesn't exist then the function can respond with the correct details", {

  # Add one match to redis
  match.data <- KEYS %>%
    footballstats::amatch_info(
      footballstats::matchData[1, ]
    )

  message <- utils::capture.output(
    KEYS %>% footballstats::missing_commentaries()
  )

  # Length of three (2 for title, 1 for missing commentaries)
  expect_equal( message %>% length, 3 )

  # Make sure the message contains the missing details
  for (i in c("home.team", "away.team")) {
    expect_true( message[3] %>% grepl(pattern = match.data[[i]]) )
  }

})
