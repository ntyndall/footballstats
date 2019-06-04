#' @title Create Football Data
#'
#' @details A function that will create football-data style results
#'  then you can build some XGB model from it
#'
#' @export


create_football_data <- function() {
  
  # Get all files
  location <- "~/Documents/footballstats/inst/football-data/"
  
  INCLUDE_ODDS <- FALSE
  
  # Get all the file names
  fNames <- location %>%
    paste0(location %>% list.files)
  
  # Initialise variables
  teamIDs <- list()
  test.metrics <- total.metrics <- data.frame(stringsAsFactors = FALSE)
  
  # Set up keys? 4 should be empty!
  KEYS <- footballstats::keys_for_testing(
    dbnum = 4
  )
  # Set up the grids
  GRIDS <- list(
    XG_BOUND = KEYS$XG_BOUND,
    DAYS = KEYS$DAYS,
    PARAM_GPOINTS = KEYS$PARAM_GPOINTS,
    PARAM_GBOUNDARY = KEYS$PARAM_GBOUNDARY,
    PARAM_DECAY = KEYS$PARAM_DECAY,
    PARAM_TOTALPER = KEYS$PARAM_TOTALPER
  )
  
  KEYS$RED$FLUSHDB()
  
  # Loop over all the file names
  for (i in 1:(fNames %>% length)) {
    cat("\n", i, "/", fNames %>% length, "\n\n")
    
    # Get full data set for a particular league
    full.data <- fNames[i] %>%
      read.table(
        sep = ";",
        header = T,
        stringsAsFactors = FALSE
      )
    
    # full.data$Season <- "2018/2019"
    # Now get all the unique seasons
    uniqueSeasons <- full.data$Season %>%
      unique
    
    # Loop over all unique seasons
    for (j in 1:(uniqueSeasons %>% length)) {
      
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
      
      # Order it
      new.data %<>% footballstats::order_matchdata(formatter = "%d/%m/%y")
      
      # Assign season and competition
      KEYS$SEASON <- uniqueSeasons[j] %>%
        strsplit(split = "/") %>%
        purrr::flatten_chr() %>%
        `[`(1)
      KEYS$COMP <- i
      KEYS$TIL <- uniqTeams %>% length
      
      # Create table
      cat(paste0(Sys.time(), ' | Creating the league table ... \n'))
      KEYS %>% footballstats::create_table(
        matchData = new.data
      )
      
      # Store positions on a weekly basis
      cat(paste0(Sys.time(), ' | Storing weekly positions ... \n'))
      KEYS %>% footballstats::weekly_positions()
      
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
        X = new.data$home.score %>% `-`(new.data$away.score),
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
      
      # Also include odds as a feature!
      # if (INCLUDE_ODDS) {
      #   odd.frme <- data.frame(stringsAsFactors = FALSE)
      #   for (o in 1:(new.metrics$data %>% nrow)) {
      #     odd.frme %<>% rbind(
      #       new.data %>%
      #         subset(zzz.matchID == new.metrics$matchIDs[o]) %>%
      #         dplyr::select(zzz.bet365Homewin, zzz.bet365Draw, zzz.bet365Awaywin)
      #     )
      #   }
      #   
      #   result.vec <- new.metrics$data$res
      #   new.metrics$data$res <- NULL
      #   
      #   if (odd.frme %>% nrow %>% `!=`(new.metrics$data %>% nrow)) {
      #     new.metrics$data %<>% cbind(
      #       data.frame(
      #         zzz.bet365Homewin = NA,
      #         zzz.bet365Draw = NA,
      #         zzz.bet365Awaywin = NA,
      #         stringsAsFactors = FALSE
      #       )
      #     )
      #   } else {
      #     new.metrics$data %<>% cbind(odd.frme)
      #   }
      #   
      #   new.metrics$data$res <- result.vec
      # }
      
      # Bind it all onto one data frame
      if (uniqueSeasons[j] == "2018/2019") {
        test.metrics %<>% rbind(new.metrics$data)
      } else {
        total.metrics %<>% rbind(new.metrics$data)
      }
    }
  }
}