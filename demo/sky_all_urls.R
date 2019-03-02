
# Define the league endpoints here
leagueEndpoints <- c(
  "premier-league",
  "championship",
  "la-liga",
  "eredivisie",
  "serie-a",
  "bundesliga",
  "ligue-1"
) %>% 
  paste0("-results/")

# 2013 -> 2018 (+1) Define years to start to end
myYears <- seq(
  from = 2013, 
  to = 2019, 
  by = 1
)

firstHalf <- myYears[1:(myYears %>% length %>% `-`(1))] 
secondHalf <- myYears[2:(myYears %>% length)]
queryYears <- firstHalf %>% paste0("-") %>% paste0(secondHalf %>% substr(3, 4))
actualYears <- firstHalf %>% paste0("/") %>% paste0(secondHalf)

# Loop over both the league, and the year!
for (i in leagueEndpoints) {
  if (i %in% (c( "premier-league",
                "championship",
                "la-liga") %>% paste0("-results/"))) next
  # New data frame for every league
  all.metrics <- data.frame(stringsAsFactors = FALSE)
  
  # Loop over years first
  for (j in 1:(queryYears %>% length)) {
    # Log which query ~ 
    cat(crayon::green(paste0("\n ", i, " :: ", queryYears[j], "\n")))
    
    # Read the data from the request
    requestData <- "https://www.skysports.com/" %>% 
      paste0(i) %>% 
      paste0(queryYears[j]) %>%
      get_html()
    
    # Just the first batch
    firstContent <- requestData %>% 
      xml2::xml_find_all(".//div") %>% 
      sky_find_urls()
    
    # Find additional content by clicking `Show More` @ bottom of page
    secondContent <- tryCatch(
      expr = {
        requestData %>%
          xml2::xml_find_all(".//script") %>%
          sky_get_logical(
            myID = "type", 
            nameMatch = "text/show-more"
          ) %>% 
          xml2::xml_text() %>% 
          xml2::read_html() %>%
          xml2::xml_find_all(".//div") %>% 
          sky_find_urls()
      
      },
      error = function(e) return(c())
    )

    # Create the endpoints to target statistics
    fullUrls <- lapply(
      X = firstContent %>% `c`(secondContent) %>% strsplit(split = "[/]"),
      FUN = function(x) {
        paste0(
          utils::head(x, -1) %>% paste(collapse = "/"),
          "/stats/",
          utils::tail(x, 1)
        )
      }
    ) %>%
      purrr::flatten_chr()

    # Set up progress bar
    pb <- utils::txtProgressBar(
      min = 0, 
      max = fullUrls %>% length,
      style = 3
    )

    # Set up the actual year
    yearToPaste <- secondHalf[j]
    # (Need to loop over now)
    for (k in 1:(fullUrls %>% length)) {
      # Update progress bas
      utils::setTxtProgressBar(
        pb = pb,
        value = k
      )

      # Get the html doc
      actualDoc <- fullUrls[k] %>%
        get_html()
      
      # Get the home and away scores!
      haScores <- actualDoc %>% 
        xml2::xml_find_all(".//span") %>%
        sky_get_logical(
          nameMatch = "match-head__score"
        ) %>%
        xml2::xml_text() %>% 
        gsub(pattern = "\n", replacement = "") %>% 
        trimws() %>%
        as.integer
      
      if (haScores %>% length %>% `==`(0)) {
        cat(crayon::red("No details for :", fullUrls[k], "\n\n"))
        next
      }
      # Names & Date
      details <- actualDoc %>% 
        xml2::xml_find_all(".//li") %>%
        sky_get_logical(
          nameMatch = "match-header__detail-item"
        ) %>% 
        xml2::xml_text()
      
      # Split the home vs away match
      haTeam <- details[1] %>% 
        strsplit(split = " vs ") %>% 
        purrr::flatten_chr()
      
      # Increment year
      if (details[2] %>% grepl(pattern = "december", ignore.case = T) %>% `&`(yearToPaste == secondHalf[j])) {
        yearToPaste %<>% `-`(1)
      }

      # Get match date
      matchDate <- paste0(details[2], " ", yearToPaste) %>%
        parsedate::parse_date() %>% 
        as.Date() %>%
        as.character


      # Set up total.metrics
      total.metrics <- data.frame(
        homeTeam = haTeam[1],
        awayTeam = haTeam[2],
        homeScore = haScores[1],
        awayScore = haScores[2],
        matchResult = if (haScores[1] %>% `>`(haScores[2])) 'W' else if (haScores[1] %>% `<`(haScores[2])) 'L' else 'D',
        matchDate = matchDate,
        season = actualYears[j],
        stringsAsFactors = FALSE
      )
      
      # Get the main stats last
      allstats <- actualDoc %>% 
        xml2::xml_find_all(".//div") %>% 
        sky_get_logical(
          nameMatch = "match-stats__item"
        )
      
      # Loop over each stat!
      total.metrics %<>% 
        sky_collect_stats(
          nodeSet = allstats
        )
      
      # Keep binding on
      all.metrics %<>% gtools::smartbind(total.metrics)
    }
  }
  
  # Now write all.metrics to csv
  write.csv2(
    x = all.metrics, 
    file = "~/Documents/footballstats/inst/sky_data/" %>% 
      paste0(i %>% substr(start = 1, stop = i %>% nchar %>% `-`(1)), ".csv"), row.names = F
  )
}
