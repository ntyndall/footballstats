



classify_commentary_from_redis <- function(keyName, returnItems) {
  results <- redisConnection$HMGET(key = keyName, 
                                   field = returnItems)

  names(results) <- returnItems
  if ("possesiontime" %in% returnItems) {
    results$possesiontime <- gsub(pattern = "%", 
                                  replacement = "", 
                                  x = results$possesiontime)
  }
  return(as.double(results))
}
