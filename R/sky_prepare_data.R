

big_fun <- function() {
allCompIDs <- list(
  `premier-league` = 1,
  championship = 2,
  `la-liga` = 3,
  eredivise = 4,
  `serie-a` = 5,
  bundesliga = 6,
  `ligue-1` = 7
)

# Read it first

system.file(package = "footballstats")

read.csv2(file = "~/Documents/footballstats/inst/sky_data/premier-league-results.csv", stringsAsFactors = F) -> plData

# Set up keys? 4 should be empty!
KEYS <- footballstats::keys_for_testing(
  dbnum = 4
)

teamIDs <- list()
matchIDStarter <- 0
full.total.metrics <- total.metrics <- data.frame(stringsAsFactors = FALSE)
# use plData
for (i in 1:(allCompIDs %>% length)) {
  # Set up (read the files!)

  plData <- read.csv2(
    file = paste0(
      "~/Documents/footballstats/inst/sky_data/",
      allCompIDs %>% names %>% `[`(i),
      "-results.csv"
    ),
    stringsAsFactors = F
  )

  # Add on the comp ID:
  plData$compID <- allCompIDs[[i]] %>%
    as.integer

  # Rename some column names
  plData %<>% dplyr::rename(
    formatted_date = matchDate,
    localteam_score = homeScore,
    visitorteam_score = awayScore
  )

  # All unique seasons in data set
  uniqueSeasons <- plData$season %>%
    unique

  # Calculate unique matchID
  matchIDs <- seq(
    from = matchIDStarter %>% `+`(1),
    length.out = plData %>% nrow,
    by = 1
  )

  # Recalculate the
  matchIDStarter <- matchIDs[matchIDs %>% length]

  # Add on the IDs
  plData$id <- matchIDs

  # Get the season as a 4 character value
  actualSeason <- uniqueSeasons %>%
    strsplit(split = "/") %>%
    purrr::map(1) %>%
    purrr::flatten_chr()

  # Get unique teams
  uniqTeams <- plData$homeTeam %>%
    c(plData$awayTeam) %>%
    unique

  # Add to list if necessary!
  for (j in 1:(uniqTeams %>% length)) {
    teamInc <- uniqTeams[j] %in% (teamIDs %>% names)
    if (teamIDs %>% length %>% `>`(0)) {
      if (teamInc %>% `!`()) {
        smallList <- teamIDs[[teamIDs %>% length]] %>% `+`(1) %>% list
        names(smallList) <- uniqTeams[j]
        teamIDs %<>% c(smallList)
      }
    } else {
      smallList <- list(1)
      names(smallList) <- uniqTeams[j]
      teamIDs %<>% c(smallList)
    }
  }

  # Assign the new IDs
  plData$localteam_id <- teamIDs[plData$homeTeam] %>% as.integer
  plData$visitorteam_id <- teamIDs[plData$awayTeam] %>% as.integer

  plData %<>% dplyr::rename(
    localteam_name = homeTeam,
    visitorteam_name = awayTeam
  )

  # Go through each season!
  for (j in 1:(uniqueSeasons %>% length)) {
    # ... content here!
    sub.data <- plData %>%
      subset(plData$season %>% `==`(uniqueSeasons[j]))

    # Calculate teams in league here
    KEYS$TIL <- sub.data$localteam_name %>%
      c(sub.data$visitorteam_name) %>%
      unique %>%
      length

    # Convert to characters!
    sub.data$localteam_id %<>% as.character
    sub.data$visitorteam_id %<>% as.character
    sub.data$id %<>% as.character

    # Order it
    sub.data %<>% order_matchdata(formatter = "%Y-%m-%d")

    # Create table
    cat(paste0(Sys.time(), ' | Creating the league table ... \n'))
    KEYS %>% create_table(
      matchData = sub.data
    )

    # Store positions on a weekly basis
    cat(paste0(Sys.time(), ' | Storing weekly positions ... \n'))
    KEYS %>% weekly_positions()

    # Loop over every row to calculate their positions
    pos.frame <- data.frame(stringsAsFactors = FALSE)
    for (k in 1:(sub.data %>% nrow)) {
      pos.frame %<>% rbind(
        footballstats::feat_position(
          KEYS = KEYS,
          matchID = sub.data$id[k],
          teamIDs = c(sub.data$localteam_id[k], sub.data$visitorteam_id[k]),
          matchDate = sub.data$formatted_date[k]
        )
      )
    }

    # Now bind positions on!
    # Bind the teams in league on, to do analysis later
    sub.data %<>% cbind(
      data.frame(
        til = KEYS$TIL,
        stringsAsFactors = FALSE
      ),
      pos.frame
    )

    # Rename everything... (FIX LATER)
    new.data <- sub.data %>% dplyr::rename(
      home.score = localteam_score,
      away.score = visitorteam_score,
      result = matchResult,
      shots_ongoal.h = home.On.target,
      shots_ongoal.a = away.On.target,
      shots_total.h = home.Total.shots,
      shots_total.a = away.Total.shots,
      possesiontime.h = home.Possession..,
      possesiontime.a = away.Possession..,
      date = formatted_date,
      away.name = visitorteam_name ,
      home.name = localteam_name,
      home.id = localteam_id,
      away.id = awayID,
      id.z = matchID,
      comp.z = compID
    )

    new.data %>% dplyr::rename(

    )
    ress <- new.data %>%
      sub_metrics(
        colNames = list(localID = "home.id", awayID = "away.id")
    )
    #####
    myres <- sub.data %>%
      footballstats::optimize_variables(
        optimizeModels = FALSE,
        GRIDS = GRIDS,
        types = "neuralnetwork"
      )
    ####
    total.metrics %<>% rbind(sub.data)
  }

  # Keep appending data on!
  full.total.metrics %<>% rbind(sub.data)
}


}

