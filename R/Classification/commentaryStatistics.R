



commentaryStatistics <- function(commentary, returnItems) {
  vals <- sapply(1:length(commentary), function(j) {
    results <- redisConnection$HMGET(key = commentary[j], 
                                     field = returnItems)
    names(results) <- returnItems
    results$possesiontime <- gsub(pattern = "%", 
                                  replacement = "", 
                                  x = results$possesiontime)
    return(as.double(results))
  })
  
  columns <- ncol(vals)
  return(sapply(1:nrow(vals), function(k) {
    sum(vals[k, 1:columns])/as.double(columns)
  }))
}
