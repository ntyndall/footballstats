


addCommentaryInfoSub <- function(competitionID, matchID, teamInfo, teamStats, commentary) {
  redisConnection$HMSET(key = paste0("cmt_commentary:", competitionID, ":", matchID, ":", teamInfo),
                        field = names(teamStats), 
                        value = as.character(teamStats))
  playerStats <- commentary$player %>% purrr::when(is.null(.) ~ data.frame(), ~ .)
  if (nrow(playerStats) > 0) {
    for (j in 1:nrow(playerStats)) {
      redisConnection$HMSET(key = paste0("cmp:", competitionID, ":", matchID, ":", playerStats[j, ]$id),
                            field = names(playerStats), 
                            value = as.character(playerStats[j, ]))
    }
  }
}