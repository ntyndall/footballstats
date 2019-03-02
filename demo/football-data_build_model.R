
# Get all files
location <- "~/Documents/footballstats/inst/football-data/"

# Get all the file names
fNames <- location %>%
  paste0(location %>% list.files)

# Initialise variables
teamIDs <- list()
total.metrics <- data.frame(stringsAsFactors = FALSE)

# Set up keys? 4 should be empty!
KEYS <- footballstats::keys_for_testing(
  dbnum = 4
)

# Loop over all the file names
for (i in 1:(fNames %>% length)) {
  cat("\n", i, "/", fNames %>% length, "\n\n")

  # Get full data set for a particular league
  full.data <- fNames[i] %>%
    read.csv2(
      stringsAsFactors = FALSE
    )

  # Now get all the unique seasons
  uniqueSeasons <- full.data$Season %>%
    unique

  # Loop over all unique seasons
  for (j in 1:(uniqueSeasons %>% length)) {
    # Subset out a particular seasons
    my.data <- full.data %>%
      subset(full.data$Season %>% `==`(uniqueSeasons[i]))

    # Now map data columns to correct headers!
    new.data <- my.data %>%
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
    new.data$home.teamID <- teamIDs[new.data$home.team] %>% as.character
    new.data$away.teamID <- teamIDs[new.data$away.team] %>% as.character

    # Order it
    new.data %<>% footballstats::order_matchdata(formatter = "%d/%m/%y")

    # Assign season and competition
    KEYS$SEASON <- uniqueSeasons[j] %>%
      strsplit(split = "/") %>%
      purrr::flatten_chr() %>%
      `[`(1)
    KEYS$COMP <- i

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
          teamIDs = c(new.data$home.teamID[k], new.data$away.teamID[k]),
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
      footballstats::sub_metrics(
        colNames = list(localID = "home.teamID", awayID = "away.teamID"),
        GRIDS = GRIDS
      )

    # Bind it all onto one data frame
    total.metrics %<>% rbind(new.metrics$data)
  }
}
