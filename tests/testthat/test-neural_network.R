context('test-neural_network.R')

test_that('Make sure neural network functions for multiple scenarios', {

  # Create Fold data
  FOLD_DATA <- footballstats::scaled.data$res %>%
    footballstats::create_folds()

  # Simple full scaled data set
  result <- footballstats::scaled.data %>%
    footballstats::neural_network(
      FOLD_DATA = FOLD_DATA,
      NN = list(
        REP = 1,
        THRESH = 0.2
      )
    )

  expect_that( result$neural, is_a('nn') )
  expect_equal( result %>% length, 8 )

  # Minus the draw
  new.scaled <- footballstats::scaled.data %>%
    subset(scaled.data$res != 'D')

  # Take out Draws, can I still get a neural network (This is an edge case!)
  result <- new.scaled %>%
    footballstats::neural_network(
      FOLD_DATA = new.scaled$res %>% footballstats::create_folds(),
      NN = list(
        REP = 1,
        THRESH = 0.2
      )
    )

  expect_that( result$neural, is_a('nn') )
  expect_equal( result %>% length, 8 )

})
