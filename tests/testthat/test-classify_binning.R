context('test-classify_binning.R')

test_that('Generate basic binnings for features of totalData', {

  binList <- footballstats::get_bins(
    totalData = totalData)
  totalMinusRes <- names(totalData) %>% setdiff('res')

  expect_that( binList %>% names() %>% sort(), equals(totalMinusRes %>% sort()) )

})
