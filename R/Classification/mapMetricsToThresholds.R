

getBinns <- function(totalData) {
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
    print(i)
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