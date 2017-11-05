

classify_bin_intervals <- function(dataSet, binList) {
  binNames <- names(binList)
  for (i in 1:length(binList)) {
    singleBin <- binList[[binNames[i]]]
    vec <- dataSet[[binNames[i]]]
    vec  <- findInterval(vec, singleBin) * (-1)
    dataSet[[binNames[i]]] <- vec
  }
  return(dataSet)
}


classify_get_bins <- function(totalData) {
  # Take integer values and divide into histogram type classes.
  itemsForSVM <- names(totalData)
  itemsForSVM <- subset(itemsForSVM, itemsForSVM != 'res')
  
  # Map current form to an integer value also.
  allForms <- strsplit(totalData$form, '')
  totalData$form <- sapply(1:length(allForms), function(x) {
    wld <- allForms[[x]]
    as.integer((sum(wld == "W")*2) + sum(wld == "D"))
  })
  
  # Create endpoints for new grouping
  totalLimits <- lapply(1:length(itemsForSVM), function(i) {
    vec <- totalData[[itemsForSVM[i]]]
    minim <- min(vec)
    maxim <- max(vec)
    diff <- maxim - minim
    bins <- diff %>% purrr::when(. >= 5 ~ 5, ~ .)
    limits <- seq(minim, maxim, diff/bins)
    return(limits)
  })
  names(totalLimits) <- itemsForSVM
  return(totalLimits)
  
}
