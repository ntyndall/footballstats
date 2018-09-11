context('test-neural_network.R')

test_that('Make sure neural network functions for multiple scenarios', {

  # Simple full scaled data set
  result <- footballstats::scaled.data %>%
    mltools::gen_nn(
      NN = list(
        REP = 1,
        THRESH = 0.2
      )
    )

  expect_that( result$model, is_a('nn') )
  expect_equal( result %>% length, 3 )

  # Minus the draw
  new.scaled <- footballstats::scaled.data %>%
    subset(scaled.data$res != 'D')

  # Take out Draws, can I still get a neural network (This is an edge case!)
  result <- new.scaled %>%
    mltools::gen_nn(
      NN = list(
        REP = 1,
        THRESH = 0.2
      )
    )

  expect_that( result$neural, is_a('nn') )
  expect_equal( result %>% length, 3 )

})
