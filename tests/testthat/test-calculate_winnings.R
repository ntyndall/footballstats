context("test-calculate_winnings")


test_that("Calculate winnings from odds - all correct", {

  # 5 Match odds frame
  five.match <- footballstats::odds.frame[1:5, ]

  # All correct
  result <- five.match %>%
    footballstats::calculate_winnings(
      winInfo = list(
        actual = "W" %>% rep(5),
        predicted = "W" %>% rep(5),
        foldint = 1:5
      )
    )

  # Win everything
  expect_equal( result, five.match$homewin %>% as.double %>% sum %>% `-`(five.match %>% nrow) )

})

test_that("Calculate winnings from odds - none correct", {

  # 5 Match odds frame
  five.match <- footballstats::odds.frame[1:5, ]

  # None correct
  result <- five.match %>%
    footballstats::calculate_winnings(
      winInfo = list(
        actual = "W" %>% rep(5),
        predicted = "L" %>% rep(5),
        foldint = 1:5
      )
    )

  # Lose £5
  expect_equal( -result, five.match %>% nrow )

})

test_that("Calculate winnings from odds - Only add up those that have been predicted", {

  # Logical vector
  lVec <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

  # 5 Match odds frame
  five.match <- footballstats::odds.frame[1:5, ]

  # All correct out of 3 predicted
  result <- five.match %>%
    footballstats::calculate_winnings(
      winInfo = list(
        actual = "W" %>% rep(3),
        predicted = "W" %>% rep(3),
        foldint = 1:5 %>% `[`(lVec)
      )
    )

  # Winning all three
  expect_equal( result, five.match$homewin %>% `[`(lVec) %>% as.double %>% sum %>% `-`(lVec %>% sum) )

  # 2 / 3 correct
  result <- five.match %>%
    footballstats::calculate_winnings(
      winInfo = list(
        actual = c("W", "D", "D"),
        predicted =  c("W", "D", "L"),
        foldint = 1:5 %>% `[`(lVec)
      )
    )

  # Win two of the three (specify which 2: W + D)
  expect_equal( result, sub.match$homewin[1] %>% as.double %>% `+`(sub.match$draw[2] %>% as.double) %>% `-`(lVec %>% sum) )

})
