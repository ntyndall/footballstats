

skysports <- httr::GET(url = "https://www.skysports.com/premier-league-results/2017-18")


allitems <- skysports$content %>% 
  rawToChar() %>%
  xml2::read_html() %>%
  xml2::xml_find_all(".//div")

# Get all attributes
allattrs <- allitems %>% xml2::xml_attrs()


i <- 40

allURLs <- c()
for (i in 1:(allattrs %>% length)) {
  currentVal <- allattrs %>% `[[`(i)
  currentNames <- currentVal %>% names
  if (currentNames %>% length %>% `==`(1)) {
    # Start to parse the data
    if (currentVal %>% as.character %>% `==`("fixres__item")) {
      getURL <- allitems[i] %>% 
        xml2::xml_find_all(".//a") %>% 
        xml2::xml_attrs() %>%
        `[[`(1)
      
      # Get the next URL
      nextURL <- getURL %>%
        `[`(getURL %>% names %>% `==`("href")) %>% 
        as.character
      allURLs %<>% c(nextURL)
    }
  }
}

# Possibly more content?
allitems <- skysports$content %>% 
  rawToChar() %>%
  xml2::read_html() %>%
  xml2::xml_find_all(".//script")

# find the right index <- 
allatts <- allitems %>% xml2::xml_attrs()
moreContent <- lapply(
  X = allatts,
  FUN = function(x) {
    currNames <- x %>% names
    if ("type" %in% (currNames)) {
      if (x %>% `[`(currNames %>% `==`("type")) %>% as.character %>% `==`("text/show-more")) {
        TRUE
      } else {
        FALSE
      }
    } else {
      FALSE
    }
  }
) %>%
  purrr::flatten_lgl() %>% 
  which


allitems[moreContent] %>% xml2::xml_text() %>% xml2::read_html() -> moreData

moreAttrs <- moreData %>% xml2::xml_find_all(".//div")
allattrs <- moreAttrs %>% xml2::xml_attrs()
for (i in 1:(allattrs %>% length)) {
  currentVal <- allattrs %>% `[[`(i)
  currentNames <- currentVal %>% names
  if (currentNames %>% length %>% `==`(1)) {
    # Start to parse the data
    if (currentVal %>% as.character %>% `==`("fixres__item")) {
      getURL <- moreAttrs[i] %>% 
        xml2::xml_find_all(".//a") %>% 
        xml2::xml_attrs() %>%
        `[[`(1)
      
      # Get the next URL
      nextURL <- getURL %>%
        `[`(getURL %>% names %>% `==`("href")) %>% 
        as.character
      allURLs %<>% c(nextURL)
    }
  }
}


spltStr <- allURLs[1] %>% strsplit(split = "/[[:digit:]+$]") %>% purrr::flatten_chr()
spltStr <- paste0(spltStr[1], "/stats/", spltStr[2])
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
