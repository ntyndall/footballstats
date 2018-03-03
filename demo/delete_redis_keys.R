

redisKeys <- c(
  paste0('cwt_l*') %>% rredis::redisKeys(),
  paste0('cw_pl*') %>% rredis::redisKeys(),
  'leagueMatchSet'
)

for (j in 1:(redisKeys %>% length)) {
  redisKeys[j] %>% rredis::redisDelete()
}
