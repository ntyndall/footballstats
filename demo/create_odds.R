

# Location of raw odds
rootDir <- getwd() %>% paste0("/raw_odds/")

# All file names
allFiles <- rootDir %>%
  paste0(rootDir %>% list.files)

# Get data set with all names
my.data <- footballstats::data.2017

# Initial the odds data frame
oddFrame <- data.frame(stringsAsFactors = FALSE)

# Loop through each file and read them
for (i in 1:(allFiles %>% length)) {

  print(i)


  # Ordered competitions
  comps <- c("1204", "1205", "1221", "1229", "1232", "1269", "1352", "1457", "1399", "1425")

  # Try to match up all the teams..
  odds.data <- read.csv2(allFiles[i], stringsAsFactors = FALSE, header = TRUE, sep =  ',')
  only.one.comp <- my.data %>% subset(my.data$comp_id == comps[i])

  if (comps[i] == "1457") {
    odds.data <- odds.data[1221:(odds.data %>% nrow), ]
  }

  # First get all *unique* (probably not) teams and assign them an ID
  allTeams <- only.one.comp$localteam_name %>% unique

  # Get matching IDs
  allIDs <- only.one.comp$localteam_id[
    sapply(
      X = allTeams,
      FUN = function(x) only.one.comp$localteam_name %>% `==`(x) %>% which %>% min
    ) %>%
      as.integer
    ]

  # Function for normalising strings a bit
  alter_str <- function(x) {
    x %>% gsub(pattern = "[[:punct:]]+", replacement = "") %>% tolower %>% iconv(from = 'UTF-8', to = "ASCII//TRANSLIT")
  }

  # Now I can alter allTeams to look normal
  allTeams %<>% alter_str()

  # Format the CSV dates
  odds.data$Date %<>% as.Date(format = "%d/%m/%y")

  # Loop over CSV now
  oddType <- if (comps[i] == "1457") {
    c("Home", "Away")
  } else {
    c("HomeTeam", "AwayTeam")
  }

  fpNames <- c("ath madrid", "buyuksehyr")
  allFPs <- c("atletico madrid", "istanbul basaksehir")

  for (j in 1:(odds.data %>% nrow)) {

    indexes <- sapply(
      X = oddType,
      FUN = function(x) {
        newStr <- odds.data[[x]][j] %>% alter_str()

        # Need to list all false positives here
        if (newStr %in% fpNames) {
          newStr <- allFPs[newStr %>% `==`(fpNames) %>% which]
        }

        if (grepl(newStr, allTeams) %>% any) {
          grepl(newStr, allTeams) %>% which %>% `[`(1)
        } else {
          stringdist::stringdist(
            a = odds.data[[x]][j] %>% alter_str(),
            b = allTeams,
            method = "cosine"
          ) %>%
            which.min
        }
      }
    ) %>%
      as.integer

    # In the order of home vs. away
    bothIDs <- allIDs[indexes]

    # Now just find a direct match (use the date just incase i.e. any playoffs etc)
    actualIndex <- odds.data$Date[j] %>%
      `==`(my.data$formatted_date) %>%
      which %>%
      intersect(my.data$localteam_id %>% `==`(bothIDs[1]) %>% which) %>%
      intersect(my.data$visitorteam_id %>% `==`(bothIDs[2]) %>% which)

    if (actualIndex %>% length %>% `==`(0)) {
      print(paste0("i = ", i, " & j = ", j))
      next
    }

    # Also get the odds
    betType <- if (comps[i] == "1457") {
      paste0("Max", c("H", "D", "A"))
    } else {
      paste0("B365", c("H", "D", "A"))
    }

    oddFrame %<>% rbind(
      data.frame(
        matchID = my.data$id[actualIndex],
        homewin = odds.data[[betType[1]]][j],
        draw = odds.data[[betType[2]]][j],
        awaywin = odds.data[[betType[3]]][j],
        stringsAsFactors = FALSE
      )
    )
  }
}
