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

## ---- echo=FALSE---------------------------------------------------------
#competition.set <- "competition:set" %>% 
#  rredis::redisSMembers() %>%
competition.set <- list(
  "1005",
  "1007",
  "1093",
  "1102",
  "..."
) %>% f_json()
print(competition.set)

## ---- echo=FALSE---------------------------------------------------------
#match.info <- "csm:1204:2017:2213059" %>% 
#  rredis::redisHGetAll() %>%
match.info <- list(
  id =  "2213059",
  comp_id = "1204",
  formatted_date = "09.12.2017",
  season = "2017/2018",
  week = "16",
  venue = "The John Smith's Stadium (Huddersfield, West Yorkshire)",
  venue_id = "15188",
  venue_city = "Huddersfield, West Yorkshire",
  status = "FT",
  timer = "",
  time = "15:00",
  localteam_id = "9220",
  localteam_name = "Huddersfield Town",
  localteam_score = "2",
  visitorteam_id = "9065",
  visitorteam_name = "Brighton & Hove Albion",
  visitorteam_score = "0",
  ht_score = "[2-0]",
  ft_score = "[2-0]",
  et_score = NA,
  penalty_local = NA,
  penalty_visitor = NA
) %>% f_json()
print(match.info)

## ---- echo=FALSE---------------------------------------------------------
print(list("2213173", "2213174", "2213175", "2213176", "...") %>% f_json())

## ---- echo=FALSE---------------------------------------------------------
#match.in.set <- "c_matchSetInfo:1204" %>% 
#  rredis::redisSMembers() %>%
match.in.set <- list(
  "2212907",
  "2212908",
  "2212909",
  "2212910",
  "..."
) %>% f_json()
print(match.in.set)

## ---- echo=FALSE---------------------------------------------------------
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

## ---- echo=FALSE---------------------------------------------------------
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

## ---- echo=FALSE---------------------------------------------------------
#team.info.keys <- "cwt_l:1204:2017*" %>% 
#  rredis::redisKeys() %>% 
team.info.keys <- list(
  "cwt_l:1204:2017:12:9423",
  "cwt_l:1204:2017:15:9158",
  "cwt_l:1204:2017:16:9053",
  "cwt_l:1204:2017:20:9426",
  "..."
) %>% f_json()
print(team.info.keys)

## ---- echo=FALSE---------------------------------------------------------
team.week.info <- list(
  TEAM = "Newcastle United",
  PTS = "23",
  GF = "21",
  GD = "-10"
) %>% f_json()
print(team.week.info)

## ---- echo=FALSE---------------------------------------------------------
league.updated <- list(
  "2243999:17002", 
  "2243932:17072", 
  "2239831:14378", 
  "2224810:10307",
  "..."
) %>% f_json()
print(league.updated)


## ---- echo=FALSE---------------------------------------------------------
positions <- list(
  `9259` = "1",
  `9260` = "2",
  `9249` = "3",
  `9092` = "4",
  `...` = "..."
) %>% f_json()
print(positions)

