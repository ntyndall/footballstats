

print('check')

library(RcppRedis)
redis <- new(RcppRedis::Redis)
redis$set('t', 1)
print(redis$get('t')
