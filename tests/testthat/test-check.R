

print('check')

library(redux)
rcon <- redux::hiredis()
rcon$SET(key = 't', value = 1)
