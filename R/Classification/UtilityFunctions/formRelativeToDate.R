


formRelativeToDate <- function(matchInfo, totalForm) {
  # Determine current match date
  currentDate <- as.integer(as.Date(matchInfo$formatted_date, '%d.%m.%Y'))
  
  # Determine total data frame of forms ordered highest to lowest
  totalForm <- totalForm[sort.int(totalForm$date, decreasing = TRUE, index.return = TRUE)$ix, ]
  
  # Calculate the relative form
  formsHaveNotOccured <- sum(currentDate <= totalForm$date)
  relativeForm <- totalForm[-c(1:formsHaveNotOccured), ]
  
  formsInOrder <- relativeForm$form
  if (length(formsInOrder) >= 3) {
    return(paste(formsInOrder[1:3], collapse = ''))
  } else {
    return(NULL) 
  }
}
