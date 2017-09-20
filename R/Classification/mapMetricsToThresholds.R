
mapMetricsToThresholds <- function(totalData) {
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
  for (i in 1:length(itemsForSVM)) {
    vec <- totalData[itemsForSVM[i]][ , 1]

    minim <- min(vec)
    maxim <- max(vec)
    diff <- maxim - minim
    bins <- diff %>% purrr::when(. >= 5 ~ 5, ~ .)
    
    limits <- seq(minim, maxim, diff/bins)
    for (j in 1:bins) {
      vec[which(vec > (limits[j] - 1e-5) & vec <= limits[j+1])] <- j * (-1)
    }
    totalData[itemsForSVM[i]][ , 1] <- vec
  }
  return(totalData)
}