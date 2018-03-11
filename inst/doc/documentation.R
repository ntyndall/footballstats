## ---- echo=FALSE---------------------------------------------------------
library(footballstats)
library(rredis)
library(magrittr)
blank <- utils::capture.output(footballstats::redis_con())
first_four <- function(z) {
  z <- lapply(1:4, function(x) z[[x]])
  return(c(z, '...'))
}
f_json <- function(y) {
  return(y %>% jsonlite::toJSON(auto_unbox = TRUE) %>% jsonlite::prettify())
}

## ------------------------------------------------------------------------
competition.set <- "competition:set" %>% 
  rredis::redisSMembers() %>%
  lapply(as.character) %>%
  first_four() %>% 
  f_json()
print(competition.set)

## ------------------------------------------------------------------------
match.info <- "csm:1204:2017:2213059" %>% 
  rredis::redisHGetAll() %>%
  lapply(as.character) %>% 
  f_json()
print(match.info)

## ------------------------------------------------------------------------
print(list("2213173", "2213174", "2213175", "2213176", "...") %>% f_json())

## ------------------------------------------------------------------------
match.in.set <- "c_matchSetInfo:1204" %>% 
  rredis::redisSMembers() %>%
  lapply(as.character) %>%
  first_four() %>% 
  f_json()
print(match.in.set)

## ------------------------------------------------------------------------
print(list("9221", "9074", "9348", "9021", "...") %>% f_json())

## ---- echo=FALSE---------------------------------------------------------
basic.info <- list(
  team_id = "9065",
  is_national = "False",
  name = "Brighton & Hove Albion",
  country = "England",
  founded = "1901",
  leagues = "1204,1198,1199",
  venue_name = "The American Express Community Stadium",
  venue_id = "86109",
  venue_surface = "grass",
  venue_address = "Village Way",
  venue_city = "Falmer, East Sussex",
  venue_capacity = "30750",
  coach_name = "C. Hughton",
  coach_id = "136328"
) %>% f_json()
print(basic.info)

## ------------------------------------------------------------------------
print(list("453145", "49530", "376371", "15512", "...") %>% f_json())

## ---- echo=FALSE---------------------------------------------------------
player.info <- list(
  name = "West Bromwich Albion",
  id = "9426",
  league = "Premier League",
  league_id = "1204",
  season = "2017/2018",
  minutes = "180",
  appearances = "2",
  lineups = "2",
  substitute_in = "0",
  substitute_out = "0",
  substitute_on_bench = "0",
  goals = "0",
  yellowcards = "0",
  yellowred = "0",
  redcards = "0"
) 
print(player.info %>% f_json())

## ------------------------------------------------------------------------
team.info.keys <- "cwt_l:1204:2017*" %>% 
  rredis::redisKeys() %>% 
  `[`(c(1:10))
print(c(team.info.keys, "...") %>% f_json())

## ------------------------------------------------------------------------
team.week.info <- "cwt_l:1204:2017:23:9287" %>%
  rredis::redisHGetAll() %>% 
  lapply(as.character)
names(team.week.info) <- c("TEAM", "PTS", "GF", "GD")
print(team.week.info %>% f_json())

## ------------------------------------------------------------------------
league.updated <- "leagueMatchSet" %>% 
  rredis::redisSMembers() %>%
  lapply(as.character) %>%
  first_four() %>% 
  f_json()
print(league.updated)

## ------------------------------------------------------------------------
positions <- "cw_pl:1204:2017:23" %>%
  rredis::redisHGetAll() %>%
  lapply(as.character) %>% 
  f_json()
print(positions)

