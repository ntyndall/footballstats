#' @title Create Head to Head Data
#'
#' @export


create_headtohead_data <- function(KEYS) {

  # headtohead list
  headtohead <- list()

  # Look at a single competitionID
  data.set <- footballstats::data.2017 %>%
    subset(footballstats::data.2017$comp_id == "1204")

  # Take a random index from at least 6 games into the season, data.set is 379 rows, so say 300
  single <- data.set[300, ]
  headtohead$single <- single

  # Chop data set to only consider all these rows
  data.set <- data.set[1:299, ]

  # Get all matches involving these two teams
  bothTeams <- c(single$localteam_id, single$visitorteam_id)

  # Create a logical vector of rows to keep
  data.set %<>% subset(data.set$localteam_id %in% bothTeams | data.set$visitorteam_id %in% bothTeams)
  headtohead$matches <- data.set

  # Make sure connected to main database, and retrieve all the commentary keys
  commKeys <- paste0("cmt_commentary:", KEYS$COMP, ":", data.set$id, ":")
  allCommentaries <- c(paste0(commKeys, data.set$localteam_id),  paste0(commKeys, data.set$visitorteam_id))

  #%>%
    lapply(FUN = function(x) x %>% paste0(bothTeams)) %>%
    purrr::flatten_chr()

  MYDB <- redux::hiredis(db = 1)
  # Retrieve all the commentaries from redis
  MYDB$HGETALL(allCommentaries[1])

  comm <- MYDB$pipeline(
    .commands = lapply(
      X = allCommentaries,
      FUN = function(x) {
        print(x)
        x %>% KEYS$PIPE$HMGET(field =   c(
          'shots_total', 'shots_ongoal', 'fouls', 'corners',
          'possesiontime', 'yellowcards'
        ))
      }
    )
  )



  # Create a data frame here??
  commentaryData <- lapply(
    X = 1:(comm %>% length),
    FUN = function(x) {
      res <- comm[[x]] %>% data.frame(stringsAsFactors = FALSE)
      names(res) <- c(
        'shots_total', 'shots_ongoal', 'fouls', 'corners',
        'possesiontime', 'yellowcards'
      )
      res
    }
  ) %>% purrr::reduce(rbind)


  headtohead$commentaryData <- commentaryData
  headtohead$commentaryKeys <- allCommentaries

  # Now just grab ALL positions
  posKeys <- paste0("cw_pl:1204:2017:*") %>%
    MYDB$KEYS() %>%
    purrr::flatten_chr()

  positions <- MYDB$pipeline(
    .commands = lapply(
      X = posKeys,
      FUN = function(x) x %>% KEYS$PIPE$HGETALL()
    )
  ) %>% lapply(footballstats::create_hash)

  names(positions) <- posKeys

  headtohead$positions <- positions
getwd()
  save(headtohead, file = "/home/niall/Desktop/football-project/footballstats/data/headtohead.rda")
}
