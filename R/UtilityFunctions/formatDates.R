


formatDates <- function(standardDateFormat) {
  day <- format(x = standardDateFormat, "%d")
  month <- format(x = standardDateFormat, "%m")
  year <- format(x = standardDateFormat, "%y")
  return(paste0(day, '.', month, '.20', year))
}
