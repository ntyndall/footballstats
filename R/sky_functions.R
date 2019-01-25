#' @title Get logical
#'
#' @export
#' 

sky_get_logical <- function(nodeSet, myID = "class", nameMatch = "") {
  specificNodes <- lapply(
    X = nodeSet %>% xml2::xml_attrs(),
    FUN = function(x) {
      myN <- x %>% names
      if (myID %in% (myN)) {
        if (x %>% `[`(myN %>% `==`(myID)) %>% as.character %>% `==`(nameMatch)) TRUE else FALSE
      } else {
        FALSE
      }
    }
  ) %>%
    purrr::flatten_lgl()
  
  return(nodeSet[specificNodes])
}

#' @title Find URLs
#'
#' @export


sky_find_urls <- function(nodeSet) {
  # Get all attributes
  allattrs <- nodeSet %>% 
    xml2::xml_attrs()
  
  allURLs <- c()
  for (i in 1:(allattrs %>% length)) {
    currentVal <- allattrs %>% `[[`(i)
    currentNames <- currentVal %>% names
    if (currentNames %>% length %>% `==`(1)) {
      # Start to parse the data
      if (currentVal %>% as.character %>% `==`("fixres__item")) {
        getURL <- nodeSet[i] %>% 
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
  
  # Return all urls back
  return(allURLs)
}


#' @title Sky Collect Stats
#' 
#' @export

sky_collect_stats <- function(total.metrics, nodeSet) {
  
  for (i in 1:(nodeSet %>% length)) {
    # Get title
    mytitle <- nodeSet %>% 
      `[`(i) %>% 
      xml2::xml_find_first(".//h5") %>% 
      xml2::xml_text() %>% 
      trimws()
    
    # just spans
    justSpans <- nodeSet %>% 
      `[`(i) %>% 
      xml2::xml_find_all(".//span")
    
    # Get home/ away attributes
    homeAway <- justSpans %>% 
      xml2::xml_attrs()
    
    only_drole <- function(x) x %>% purrr::map(function(x) "data-role" %in% (x %>% names)) %>% purrr::flatten_lgl()
    justSpans %<>% `[`(homeAway %>% only_drole())
    homeAway %<>% `[`(homeAway %>% only_drole())
    
    # Loop over both
    for (j in 1:(justSpans %>% length)) {
      singleNames <- homeAway[[j]] %>% names
      droleAttr <- homeAway %>% `[[`(j) %>% `[`("data-role" %>% `==`(singleNames)) %>% as.character
      if (droleAttr %>% `==`("match-stat-home")) homeFig <- justSpans[j] else awayFig <- justSpans[j]
    }
    
    toBind <- data.frame(homeFig %>% xml2::xml_text(), awayFig %>% xml2::xml_text())
    names(toBind) <- paste0(c("home.", "away."), mytitle)
    
    total.metrics %<>% cbind(toBind)
  }
  
  # Return updated data frame back
  return(total.metrics)
}

#' @title Sky Get HTML
#'
#' @export


sky_get_html <- function(x) {
  return(
    x %>%
      httr::GET() %>%
      `[[`("content") %>% 
      rawToChar() %>%
      xml2::read_html()
  )
}
