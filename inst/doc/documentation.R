## ---- echo=FALSE---------------------------------------------------------
library(footballstats)
library(rredis)
library(magrittr)
blank <- utils::capture.output(footballstats::redis_con())
first_four <- function(z) {
  z <- lapply(1:4, function(x) z[[x]])
  return(c(z, '...'))
}

## ------------------------------------------------------------------------
match.info <- "csm:1204:2017:2213059" %>% 
  rredis::redisHGetAll() %>%
  lapply(as.character)
print(match.info)

## ------------------------------------------------------------------------
competition.set <- "competition:set" %>% 
  rredis::redisSMembers() %>%
  lapply(as.character) %>%
  first_four()
print(competition.set)

## ------------------------------------------------------------------------
print(list("9221", "9074", "9348", "9021", "..."))

## ------------------------------------------------------------------------
match.in.set <- "c_matchSetInfo:1204" %>% 
  rredis::redisSMembers() %>%
  lapply(as.character) %>%
  first_four()
print(match.in.set)

## ------------------------------------------------------------------------
print(list("2213173", "2213174", "2213175", "2213176", "..."))

## ------------------------------------------------------------------------
league.updated <- "leagueMatchSet" %>% 
  rredis::redisSMembers() %>%
  lapply(as.character) %>%
  first_four()
print(league.updated)

## ------------------------------------------------------------------------
team.info.keys <- "cwt_l:1204:2017*" %>% 
  rredis::redisKeys() %>% 
  `[`(c(1:10))
print(c(team.info.keys, "..."))

## ------------------------------------------------------------------------
team.week.info <- "cwt_l:1204:2017:23:9287" %>%
  rredis::redisHGetAll() %>% 
  lapply(as.character)
names(team.week.info) <- c("TEAM", "PTS", "GF", "GD")
print(team.week.info)

## ------------------------------------------------------------------------
positions <- "cw_pl:1204:2017:23" %>%
  rredis::redisHGetAll() %>%
  lapply(as.character)
print(positions)

## ------------------------------------------------------------------------
league.updated <- "leagueMatchSet" %>% 
  rredis::redisSMembers() %>%
  lapply(as.character) %>%
  first_four()
print(league.updated)

