getTimeIntervals <- function() {
  lastMatchTime <- redis$GET(key = 'match:lastInterval')
  if (is.null(lastMatchTime)) {
    lastMatchTime <- Sys.Date() - (365 * 2)
    # Adjust to make this fall on a Friday.
    while (weekdays(lastMatchTime) != 'Friday') {
      lastMatchTime <- lastMatchTime + 1
    }
    redis$SET(key = 'match:lastInterval',
              value = as.integer(lastMatchTime))
  } else {
    as.Date(lastMatchTime, origin = "1970-01-01")
  }
  return(lastMatchTime)
}
