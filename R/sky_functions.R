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
