

# Make the query
skysports <- "https://www.skysports.com/premier-league-results/2017-18" %>%
  httr::GET()

# Read the data from the request
requestData <- skysports %>% 
  `[[`("content") %>%
  rawToChar() %>%
  xml2::read_html()

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

# Combine the two vectors to get a complete list of URLs
allUrls <- c(firstContent, secondContent)

# Create the endpoints to target statistics
fullUrls <- lapply(
  X = allUrls %>% strsplit(split = "/[[:digit:]+$]"), 
  FUN = function(x) paste0(x[1], "/stats/", x[2])
) %>%
  purrr::flatten_chr()

# (Need to loop over now)
httr::GET(url = spltStr) -> actualMatch

actualDoc <- actualMatch %>% xml2::read_html()
allDivs <- actualDoc %>% xml2::xml_find_all(".//div")


allstats <- lapply(
  X = allDivs %>% xml2::xml_attrs(),
  FUN = function(x) {
    allNames <- x %>% names
    if ("class" %in% (allNames)) {
      if (x %>% `[`(allNames %>% `==`("class")) %>% as.character %>% `==`("match-stats__item")) {
        TRUE
      } else {
        FALSE
      }
    } else {
      FALSE
    }
  }
) %>% 
  purrr::flatten_lgl()


allDivs[allstats] %>% xml2::xml_text()
allstats


# Get meta data first!!
myScores <- actualDoc %>% xml2::xml_find_all(".//span")

# Get the score
allstats1 <- lapply(
  X = myScores %>% xml2::xml_attrs(),
  FUN = function(x) {
    allNames <- x %>% names
    if ("class" %in% (allNames)) {
      if (x %>% `[`(allNames %>% `==`("class")) %>% as.character %>% `==`("match-head__score")) {
        TRUE
      } else {
        FALSE
      }
    } else {
      FALSE
    }
  }
) %>% 
  purrr::flatten_lgl()

# Get the home and away scores!
haScores <- myScores[allstats1] %>% 
  xml2::xml_text() %>% 
  gsub(pattern = "\n", replacement = "") %>% trimws()


# Names & Date
myTeams <- actualDoc %>% xml2::xml_find_all(".//li")
allstats3 <- lapply(
  X = myTeams %>% xml2::xml_attrs(),
  FUN = function(x) {
    allNames <- x %>% names
    if ("class" %in% (allNames)) {
      if (x %>% `[`(allNames %>% `==`("class")) %>% as.character %>% `==`("match-header__detail-item")) {
        TRUE
      } else {
        FALSE
      }
    } else {
      FALSE
    }
  }
) %>% 
  purrr::flatten_lgl()
myTeams[allstats3] %>% xml2::xml_text() -> details

haTeam <- details[1] %>% strsplit(split = " vs ") %>% purrr::flatten_chr()
matchDate <- parsedate::parse_date(details[2]) %>% as.Date()




total.data.metrics <- data.frame(
  homeTeam = haTeam[1],
  awayTeam = haTeam[2],
  matchDate = matchDate,
  season = "2017/2018",
  stringsAsFactors = FALSE
)

for (j in 1:(allDivs[allstats] %>% length)) {
  # Get title
  mytitle <- allDivs[allstats] %>% `[`(1) %>% xml2::xml_find_first(".//h5") %>% xml2::xml_text() %>% trimws()
  myNames <- paste0(c("home.", "away."), mytitle)
  # just spans
  justSpans <- allDivs[allstats] %>% `[`(1) %>% xml2::xml_find_all(".//span")
  
  homeAway <- justSpans %>% xml2::xml_attrs()
  for (i in 1:(homeAway %>% length)) {
    singleNames <- homeAway[[i]] %>% names
    if ("data-role" %in% (singleNames)) {
      homeAway[[i]]["data-role" %>% `==`(singleNames)] %>% as.character
      if ("match-stat-home") {
        homeFig <- justSpans[2] %>% xml2::xml_text()
      } else {
        awayFig <- justSpans[4] %>% xml2::xml_text()
      }
    }
  }
  toBind <- data.frame(homeFig, awayFig)
  names(toBind) <- myNames
  
  total.data.metrics %<>% cbind(toBind)
}
