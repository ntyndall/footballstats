#' @title Create Football Data
#'
#' @details A function that will create football-data style results
#'  then you can build some XGB model from it
#'
#' @export


### WHY  FOR I = 5 (E1) IS THERE A MISMATCH IN THE DATA FRAMES?!

create_football_data <- function(GRIDS, type = 'all') {

  # Get all files
  location <- "~/Documents/footballstats/inst/football-data/" %>%
    paste0(if (type == 'all') '' else '2018-2019/')


  INCLUDE_ODDS <- FALSE

  # Get all the file names
  fNames <- location %>%
    paste0(location %>% list.files(pattern = '.csv'))

  # Initialise variables
  teamIDs <- list()
  meta.metrics <- total.metrics <- data.frame(stringsAsFactors = FALSE)

  # Set up keys? 4 should be empty!
  KEYS <- footballstats::keys_for_testing(
    dbnum = 4
  )

  # Reset database for good measure
  KEYS$RED$FLUSHDB()

  # Loop over all the file names
  for (i in 1:(fNames %>% length)) {

    cat("\n", i, "/", fNames %>% length, "\n\n")

    # Get full data set for a particular league
    if (type == 'all') {
      full.data <- fNames[i] %>%
        read.table(
          sep = ";",
          header = T,
          stringsAsFactors = FALSE
        )
    } else {
      my.d <- fNames[i] %>% readLines()

      dataList <- my.d %>% strsplit(split = ",")

      full.data <- dataList[2:(dataList %>% length)] %>%
        lapply(function(x) x %>% t %>% data.frame(stringsAsFactors = FALSE)) %>%
        purrr::reduce(rbind)

      names(full.data) <- dataList[[1]]

      uniqueSeasons <- full.data$Season <- "2018/2019"
    }

    # Now get all the unique seasons
    uniqueSeasons <- full.data$Season %>%
      unique

    # Loop over all unique seasons
    for (j in 1:(uniqueSeasons %>% length)) {

      print(paste0(j, " :: ", uniqueSeasons %>% length))
      # Subset out a particular seasons
      season.data <- full.data %>%
        subset(Season %>% `==`(uniqueSeasons[j]) %>% `&`(Date %>% `!=`("")))

      # Now map data columns to correct headers!
      new.data <- season.data %>%
        footballstats::rename_columns(
          mapping = "footballdata"
        )

      # Get unique teams!
      uniqTeams <- new.data$home.team %>%
        c(new.data$away.team) %>%
        unique

      # Create new teamIDs
      for (k in 1:(uniqTeams %>% length)) {
        teamInc <- uniqTeams[k] %in% (teamIDs %>% names)
        if (teamIDs %>% length %>% `>`(0)) {
          if (teamInc %>% `!`()) {
            smallList <- teamIDs[[teamIDs %>% length]] %>% `+`(1) %>% list
            names(smallList) <- uniqTeams[k]
            teamIDs %<>% c(smallList)
          }
        } else {
          smallList <- list(1)
          names(smallList) <- uniqTeams[k]
          teamIDs %<>% c(smallList)
        }
      }

      # Add matchID's on
      new.data$zzz.matchID <- 1:(new.data %>% nrow) %>% as.character

      # Assign the new IDs
      new.data$home.id <- teamIDs[new.data$home.team] %>% as.character
      new.data$away.id <- teamIDs[new.data$away.team] %>% as.character

      # This needs to be more sophisticated at some point!
      yearFormat <- if (new.data$zzz.date %>% nchar %>% mean %>% `<`(9)) "%y" else "%Y"

      # Order it
      new.data %<>% footballstats::order_matchdata(formatter = paste0("%d/%m/", yearFormat))

      # Assign season and competition
      KEYS$SEASON <- uniqueSeasons[j] %>%
        strsplit(split = "/") %>%
        purrr::flatten_chr() %>%
        `[`(1)
      KEYS$COMP <- i
      KEYS$TIL <- uniqTeams %>% length

      # Create table
      cat(paste0(Sys.time(), ' | Creating the league table ... \n'))
      KEYS %>% create_table(
        matchData = new.data
      )

      # Store positions on a weekly basis
      cat(paste0(Sys.time(), ' | Storing weekly positions ... \n'))
      KEYS %>% weekly_positions()

      # Loop over every row to calculate their positions
      pos.frame <- data.frame(stringsAsFactors = FALSE)
      for (k in 1:(new.data %>% nrow)) {
        pos.frame %<>% rbind(
          footballstats::feat_position(
            KEYS = KEYS,
            matchID = new.data$zzz.matchID[k],
            teamIDs = c(new.data$home.id[k], new.data$away.id[k]),
            matchDate = new.data$zzz.date[k]
          )
        )
      }

      # Length of the unique teams!
      new.data$zzz.til <- uniqTeams %>% length

      # Now append the positions on
      new.data %<>% cbind(pos.frame)

      # Append the results on
      new.data$zzz.result <- sapply(
        X = new.data$home.score %>% as.integer %>% `-`(new.data$away.score %>% as.integer),
        FUN = function(x) if (x %>% `>`(0)) "W" else if (x == 0) "D" else "L"
      )

      # Get the actual metrics required
      new.metrics <- new.data %>%
        sub_metrics(
          colNames = list(
            localID = "home.id",
            awayID = "away.id"
          ),
          GRIDS = GRIDS
        )

      # get meta data..
      temp.meta <- new.data %>%
        subset(zzz.matchID %in% new.metrics$matchIDs)

      # If something has gone wrong??
      if (temp.meta %>% nrow %>% `!=`(new.metrics$data %>% nrow)) {
        next
      }

      # Only select those necessary
      temp.meta %<>%
        dplyr::select(
          zzz.division,
          zzz.date,
          zzz.bet365Homewin,
          zzz.bet365Draw,
          zzz.bet365Awaywin,
          zzz.season
        )

      meta.metrics %<>% rbind(temp.meta)

       new.metrics$data$date <- temp.meta %>%
        `[[`('zzz.date') %>%
        format('%m') %>%
        as.integer

      # Make sure it is last!
      myresults <- new.metrics$data$res
      new.metrics$data$res <- NULL
      new.metrics$data$res <- myresults

      total.metrics %<>% rbind(new.metrics$data)
      # # Bind it all onto one data frame
      # if (uniqueSeasons[j] == "2018/2019") {
      #   # Bind on the data
      #   #test.metrics %<>% rbind(new.metrics$data)
      #
      #   # Bind on the odds
      #   temp.frame <- new.data %>%
      #     subset(zzz.matchID %in% new.metrics$matchIDs)
      #
      #   # Bind this temporary frame on
      #   meta.metrics %<>% rbind(temp.frame)
      # } else {
      #   total.metrics %<>% rbind(new.metrics$data)
      # }
    }
  }

  # Return all the metrics back
  return(
    list(
      train = total.metrics,
      meta = meta.metrics
    )
  )
}
