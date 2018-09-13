context("test-optimize_decay")


test_that("Make sure the decay functionality works", {

  result <- footballstats::optimize_decay(3, 0.1)


  expect_equal( footballstats::optimize_decay(3, 0.1) %>% sum, 1 )
  expect_equal( footballstats::optimize_decay(3, 0.1) %>% length, 3 )
  expect_equal( footballstats::optimize_decay(5, 0.1) %>% sum, 1 )
  expect_equal( footballstats::optimize_decay(5, 0.1) %>% length, 5 )
  expect_equal( footballstats::optimize_decay(1, 0.01) %>% sum, 1 )
  expect_equal( footballstats::optimize_decay(1, 0.01) %>% length, 1 )

  equalDecayLen <- 2
  equalDecay <-  1 %>% `/`(equalDecayLen) %>% rep(equalDecayLen)
  expect_equal( footballstats::optimize_decay(equalDecayLen, 5000), equalDecay )

})
