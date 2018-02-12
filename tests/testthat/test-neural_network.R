context('test-neural_network.R')

test_that('Make sure neural network functions for multiple scenarios', {

  # Simple full scaled data set
  result <- footballstats::scaled.data %>%
    footballstats::neural_network()

  expect_that( result, is_a('nn') )
  expect_equal( result %>% length, 13)

  # Take out Draws, can I still get a neural network (This is an edge case!)
  result <- footballstats::scaled.data %>%
    subset(scaled.data$res != 'D') %>%
    footballstats::neural_network()

  expect_that( result, is_a('nn') )
  expect_equal( result %>% length, 13)

})
