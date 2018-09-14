context("test-optimize_variables")


test_that("Create a model (neural network) during optimization", {
  # Set up some grid
  GRIDS <- list(
    DAYS = 3,
    GRID_PTS = 8,
    GRID_BOUND = 0.2,
    DECAY = 5000,
    TOTAL_PERC = 1,
    NN_REP = 1,
    NN_THRESH = 0.005,
    XG_ROUNDS = 100,
    XG_DEPTH = 10,
    XG_ETA = 0.2,
    XG_GAMMA = 2
  )

  # Take a slice of all the metrics
  myres <- footballstats::total.metrics[1:300, ] %>%
    footballstats::optimize_variables(
      optimizeModels = FALSE,
      GRIDS = GRIDS,
      types = "neuralnetwork"
    )

  expect_equal( myres %>% length, 1 )
  expect_equal( myres$neuralnetwork %>% length, 3)
  expect_is( myres$neuralnetwork$model, "nn" )
  expect_equal( myres$neuralnetwork$totalStats %>% length, 7)

})
