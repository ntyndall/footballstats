#' @title Build Raw Data
#'
#' @export


build_raw_data <- function(KEYS, singleFixture) {
  # Get team information from fixture data frame
  matchID <- singleFixture$id %>% as.integer
  teamIDs <- c(singleFixture$localteam_id, singleFixture$visitorteam_id)

  # Allowed Commentaries
  allowedCommentaries <-  c(
    'shots_total', 'shots_ongoal', 'fouls', 'corners',
    'possesiontime', 'yellowcards'
  )

  # Basic fields to return
  retFields <- c(
    "id", "formatted_date", "localteam_name", "localteam_id", "visitorteam_name",
    "visitorteam_id", "localteam_score", "visitorteam_score"
  )

  commKey <- paste0('cmt_commentary:', KEYS$COMP, ":")

  # Need to build a similar list of data frames...
  idTypes <- c('localID', 'awayID')

  # May need to up this (or change it in general depending on the model)
  DAYNUM <- 3

  allresults <- lapply(
    X = 1:2,
    FUN = function(i) {
      currentID <- if (i == 1) teamIDs[1] else teamIDs[2]

      # Warning: Need to handle not enough commentaries here!
      cKeys <- commKey %>%
        paste0("*:", currentID) %>%
        KEYS$RED$KEYS() %>%
        purrr::flatten_chr()

      ordKeys <- KEYS %>% footballstats::order_commentaries(
        commentaryKeys = cKeys
      ) %>%
        rev

      # Get the matchIDs
      matchIDs <- ordKeys %>%
        footballstats::flatt(y = 3)

      # Make sure matchID is next in the sequence
      subIDs <- matchIDs %>% `<`(matchID)

      # Subset IDs and keys
      ordKeys %<>% `[`(subIDs)
      matchIDs %<>% `[`(subIDs)

      # See
      localOrAway <- KEYS$RED$pipeline(
        .commands = lapply(
          X = paste0("csm:", KEYS$COMP, ":", KEYS$SEASON, ':', matchIDs),
          FUN = function(x) x %>% KEYS$PIPE$HGET("localteam_id")
        )
      ) %>%
        purrr::flatten_chr()

      localMatch <- localOrAway %>% `==`(currentID) %>% which
      awayMatch <- c(1:(localOrAway %>% length)) %>% setdiff(localMatch) %>% `[`(c(1:DAYNUM))
      localMatch %<>% `[`(c(1:DAYNUM))

      # Now, what do I need in each data frame?!?!
      localComm <- ordKeys %>% `[`(localMatch)
      awayComm <- ordKeys %>% `[`(awayMatch)

      localMatchIDs <- matchIDs %>% `[`(localMatch)
      awayMatchIDs <- matchIDs %>% `[`(awayMatch)

      # For local and away
      mf <- function(x, y) x %>% purrr::map(y) %>% purrr::flatten_chr()

      myres <- lapply(
        X = 1:2,
        FUN = function(j) {

          # Match types [home / away]
          currentIDs <- if (j == 1) localMatchIDs else awayMatchIDs

          # Get base results
          baseResults <- KEYS$RED$pipeline(
            .commands = lapply(
              X = paste0("csm:", KEYS$COMP, ":", KEYS$SEASON, ":", currentIDs),
              FUN = function(x) x %>% KEYS$PIPE$HMGET(field = retFields)
            )
          ) %>%
            purrr::map(function(x) x %>% purrr::flatten_chr())

          # Interleave the two vectors so home and away are side by side
          subCKeys <- commKey %>%
            paste0(currentIDs, ":", c(baseResults %>% mf(4), baseResults %>% mf(6))) %>%
            matrix(ncol = 2)

          subCKeys <- sapply(
            X = 1:(subCKeys %>% nrow),
            FUN = function(x) subCKeys[x, ]
          ) %>%
            as.character

          cResults <- lapply(
            X = subCKeys,
            FUN = function(x) x %>% footballstats::commentary_from_redis(returnItems = allowedCommentaries)
          )

          # Can I stack results every 2?
          newRes <- lapply(X = 1:(cResults[[1]] %>% length), FUN = function(x) cResults %>% purrr::map(x) %>% purrr::flatten_dbl())
          names(newRes) <- allowedCommentaries

          tf <- newRes %>% data.frame(stringsAsFactors = FALSE)
          tf <- cbind(tf %>% subset(c(TRUE, FALSE)), tf %>% subset(c(FALSE, TRUE)))
          names(tf) <- sapply(c(".h", ".a"), FUN = function(x) allowedCommentaries %>% paste0(x)) %>% as.character

          # Now create basic data frame
          newBase <- lapply(X = 1:(baseResults[[1]] %>% length), FUN = function(x) baseResults %>% purrr::map(x) %>% purrr::flatten_chr())
          newBase %<>% data.frame(stringsAsFactors = FALSE)
          names(newBase) <- c(
            "matchID", "date", "localName", "localID", "awayName",
            "awayID", "localScore", "awayScore"
          )

          # Convert some stuff...
          newBase$date %<>% as.Date(format ='%d.%m.%Y')
          newBase$localScore %<>% as.double
          newBase$awayScore %<>% as.double

          # Get scores
          newBase$result <- sapply(
            X = 1:DAYNUM,
            FUN = function(x) newBase$localScore[x] %>% footballstats::match_result(newBase$awayScore[x])
          )

          allData <- cbind(newBase, tf)

          # Now get positions and other stuff
          positions <- lapply(
            X = 1:DAYNUM,
            FUN = function(x) {
              KEYS %>% footballstats::feat_position(
                matchID = allData$matchID[x],
                teamIDs = c(allData$localID[x], allData$awayID[x]),
                matchDate = allData$date[x]
              )
            }
          ) %>%
            purrr::reduce(rbind)

          allData %<>% cbind(positions)
          allData$til <- KEYS$TIL
          allData
        }
      )
      myres
    }
  )

  # Return results back as a data frame
  return(allresults %<>% purrr::flatten_dfr())
}
