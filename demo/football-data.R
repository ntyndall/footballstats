
# Load all details
allDetails <- "~/Documents/footballstats/inst/mappings/data-structures.yaml" %>%
  yaml::yaml.load_file() %>%
  `[[`("footballdata")

# Set locale for illegal characters!
Sys.setlocale('LC_ALL', 'C')

icount <- 0
# Loop over everything
for (league in allDetails) {

  icount %<>% `+`(1)
  # Get each of the leagues!
  leagues <- league %>%
    `[[`("leagues") %>%
    as.character
  league %>% `[[`("leagues") %>% as.character

  # 2013 -> 2018 (+1) Define years to start to end
  myYears <- seq(
    from = league %>% `[[`("startYear"),
    to = 2019,
    by = 1
  )

  yrQuery <- myYears %>%
    substr(3, 4) %>%
    utils::head(-1) %>%
    paste0(myYears %>% substr(3, 4) %>% utils::tail(-1))

  cat(crayon::black("\n (", icount, "/", allDetails %>% length, ") "))
  for (i in 1:(leagues %>% length)) {
    all.metrics <- data.frame(stringsAsFactors = FALSE)
    cat(crayon::green("\n", i, " / ", leagues %>% length, " \n"))
    cat(crayon::cyan(yrQuery %>% length, " "))

    # If the file exists then go next!
    fExists <- "~/Documents/footballstats/inst/football-data/" %>%
      paste0(leagues[i], ".csv") %>%
      file.exists
    if (fExists) next

    for (j in 1:(yrQuery %>% length)) {

      # Progress
      cat(j, " ")

      reqContent <- "http://www.football-data.co.uk/mmz4281/" %>%
        paste0(yrQuery[j], "/") %>%
        paste0(leagues[i], ".csv") %>%
        httr::GET() %>%
        `[[`("content") %>%
        rawToChar() %>%
        strsplit(split = "\r\n") %>%
        purrr::flatten_chr()

      reqRes <- reqContent %>%
        utils::tail(-1) %>%
        purrr::map(function(x) x %>% strsplit(split = ",") %>% purrr::flatten_chr() %>% t %>% data.frame()) %>%
        purrr::reduce(gtools::smartbind)

      # Header names
      headerNames <- reqContent[1] %>%
        strsplit(split = ",") %>%
        purrr::flatten_chr()

      # Check for any empty column names..
      emptyCols <- headerNames %>% `==`("")

      if (emptyCols %>% any) {
        headerNames %<>% `[`(emptyCols %>% `!`())
        reqRes %<>% `[`(emptyCols %>% `!`())
      }
      # Subset only columns which have a valid header name
      reqRes %<>% `[`(1:(headerNames %>% length))

      # Now cast the header names
      names(reqRes) <- headerNames

      # Add season onto the data frame
      reqRes$Season <- myYears[j] %>%
        paste0("/") %>%
        paste0(myYears[j + 1])

      # Bind all similar leagues
      all.metrics %<>% gtools::smartbind(reqRes)
    }

    write.csv2(
      x = all.metrics,
      file = "~/Documents/footballstats/inst/football-data/" %>%
        paste0(leagues[i], ".csv"), row.names = F
    )
  }
}


# Now clean it up!!
# Read in all the files and find commonality!
#all.metrics[1, ] %>% rename_data(mapping = "footballdata")

