


findBinIntervals <- function(dataSet, binList) {
  binNames <- names(binList)
  for (i in 1:length(binList)) {
    singleBin <- binList[[binNames[i]]]
    vec <- dataSet[[binNames[i]]]
    vec  <- findInterval(vec, singleBin) * (-1)
    dataSet[[binNames[i]]] <- vec
  }
  return(dataSet)
}
