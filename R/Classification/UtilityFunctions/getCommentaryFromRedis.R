



getCommentaryFromRedis <- function(keyName, returnItems) {
  results <- redisConnection$HMGET(key = keyName, 
                                   field = returnItems)

  names(results) <- returnItems
  results$possesiontime <- gsub(pattern = "%", 
                                replacement = "", 
                                x = results$possesiontime)
  return(as.double(results))
}
