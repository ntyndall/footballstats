#' @title Redis Mapping
#'
#' @description A function that maps the rredis capability to the
#'  same format as redux method calls to allow for proper testing.
#'
#' @param None.
#'
#' @return A list of Redis commands such as `SET` or `GET`. Once the
#'  function redisMapping() is assigned to `x`, then the methods are
#'  available through the usual x$GET, x$SET etc.
#'


redisMapping <- function() {
  return(list(SET = function(key, value) { rredis::redisSet(key = key, value = value) },
              GET = function(key) { rredis::redisGet(key = key) },
              HMSET = function(key, field, value) { listToSet <- as.list(value);
                                                    names(listToSet) <- field;
                                                    rredis::redisHMSet(key = key,
                                                                       values = listToSet) },
              LPUSH = function(key, value) { rredis::redisLPush(key = key, value = value) },
              SADD = function(key, member) { rredis::redisSAdd(set = key, element = member) },
              LPOP = function(key) { rredis::redisLPop(key = key) },
              EXISTS = function(key) { rredis::redisExists(key = key) }))
}
