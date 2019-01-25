
# Read the data from the request
requestData <- "https://www.skysports.com/premier-league-results/2017-18" %>% 
  get_html()

# Just the first batch
firstContent <- requestData %>% 
  xml2::xml_find_all(".//div") %>% 
  sky_find_urls()

# Find additional content by clicking `Show More` @ bottom of page
secondContent <- requestData %>%
  xml2::xml_find_all(".//script") %>%
  sky_get_logical(
    myID = "type", 
    nameMatch = "text/show-more"
  ) %>% 
    xml2::xml_text() %>% 
    xml2::read_html() %>%
    xml2::xml_find_all(".//div") %>% 
    sky_find_urls()

# Create the endpoints to target statistics
fullUrls <- lapply(
  X = firstContent %>% `c`(secondContent) %>% strsplit(split = "/[[:digit:]+$]"), 
  FUN = function(x) paste0(x[1], "/stats/", x[2])
) %>%
  purrr::flatten_chr()

# (Need to loop over now)
all.metrics <- data.frame(stringsAsFactors = FALSE)
for (i in 1:(fullUrls %>% length)) {
  # Get the html doc
  actualDoc <- fullUrls[j] %>%
    get_html()

  # Get the home and away scores!
  haScores <- actualDoc %>% 
    xml2::xml_find_all(".//span") %>%
    sky_get_logical(
      nameMatch = "match-head__score"
    ) %>%
      xml2::xml_text() %>% 
      gsub(pattern = "\n", replacement = "") %>% 
      trimws()
  
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

  # Set up total.metrics
  total.metrics <- data.frame(
    homeTeam = haTeam[1],
    awayTeam = haTeam[2],
    matchDate = details[2] %>% parsedate::parse_date() %>% as.Date(),
    season = "2017/2018",
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
  all.metrics %<>% rbind(total.metrics)
}
