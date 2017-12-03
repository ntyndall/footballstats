context('test-classify_binning.R')

test_that('Generate basic binnings for features of totalData', {

  totalData <- footballstats::totalData
  binList <- footballstats::get_bins(
    totalData = totalData)
  totalMinusRes <- totalData %>% names %>% setdiff('res')

  expect_that( binList %>% names() %>% sort(), equals(totalMinusRes %>% sort()) )

})

test_that('Generate basic binnings for features of totalData', {

  totalData$form <- footballstats::form_to_int(
    oldForms = totalData$form)

  newTotalForm <- footballstats::bin_intervals(
    dataSet = totalData,
    binList = binList)

  expect_that( (newTotalForm$shots_total < 0) %>% all, is_true() )

})
