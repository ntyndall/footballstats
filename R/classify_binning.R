
#'
#' @export


bin_intervals <- function(dataSet, binList) {
  binNames <- names(binList)
  for (i in 1:length(binList)) {
    singleBin <- binList[[binNames[i]]]
    vec <- dataSet[[binNames[i]]]
    vec  <- findInterval(vec, singleBin) * (-1)
    dataSet[[binNames[i]]] <- vec
  }
  return(dataSet)
}

#'
#' @export

get_bins <- function(totalData) {
  # Take integer values and divide into histogram type classes.
  itemsForSVM <- names(totalData)
  itemsForSVM <- subset(itemsForSVM, itemsForSVM != 'res')

  # Map current form to an integer value also.
  allForms <- strsplit(totalData$form, '')
  sum_form <- function(wld, f, p) `*`(`==`(wld, f) %>% sum, p)
  totalData$form <- sapply(1:length(allForms), function(x) {
    `+`(sum_form(allForms[[x]], 'W', 2), sum_form(allForms[[x]], 'D', 1)) %>%
      as.integer
  })

  # Create endpoints for new grouping
  totalLimits <- lapply(1:length(itemsForSVM), function(i) {
    vec <- totalData[[itemsForSVM[i]]]
    minim <- vec %>% min
    maxim <- vec %>% max
    len <- vec %>% unique %>% length
    diff <- maxim - minim
    bins <- ifelse(len >= 6, yes = 5, no = len)
    limits <- seq(minim, maxim, diff/bins)
    return(limits)
  })
  names(totalLimits) <- itemsForSVM
  return(totalLimits)

}
