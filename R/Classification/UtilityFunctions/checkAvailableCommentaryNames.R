



checkAvailableCommentaryNames <- function(commentaryKeys, excludeNames = c('table_id')) {
  allAvailable <- c()
   for (x in 1:length(commentaryKeys)) {
    results <- as.character(redisConnection$HGETALL(key = commentaryKeys[x]))
    cNames <- results[c(TRUE, FALSE)]
    cValues <- results[c(FALSE, TRUE)]
    empties <- cValues == ""
    
    # Remove any empty string fields
    if (any(empties)) {
      cNames <- cNames[-which(empties)]
    }
    
    # Remove any predefined variables that should never be used
    intersection <- intersect(cNames, excludeNames)
    if (!identical(intersection, character(0))) {
      cNames <- cNames[-match(c(intersection), cNames)]
    }
    
    if (x == 1) {
      allAvailable <- cNames
    } else {
      allAvailable <- intersect(cNames, allAvailable)
    }
  }
  return(allAvailable)
}
