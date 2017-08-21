#' @title Initialize Daemon
#'
#' @description ...
#'  
#' @details ...
#' 
#' @param loopOver A boolean that initiates the main outer loop to analyse
#'  data.
#' @param loopOverToday A boolean that decides when to move onto the next day
#'  for querying. So a fixed date can be input and polled over.
#' @param sleepTime An integer value that defines the wait time in seconds.
#' @param timeToClassify An integer defining the number of weeks of which after
#'  to start classifying and predicting results.
#'
#' @return Returns nothing.
#'


initializeDaemon <- function(loopOver = TRUE, loopOverToday = TRUE,
                             sleepTime = 60 * 60 * 2, timeToClassify = 7) {

  # Initialize log file
  logFile <- file("~/test.log", open = "a")

  # Define dates to kick start loop
  as.Date(as.integer(Sys.Date()), origin = '1970-01-01')
  currentDate <- 17386
  classificationStartTime <- currentDate + (timeToClassify * 7)
  timeToClassify <- FALSE
  
  # Begin infinite while loop to continuously run
  # Set the global variable `loopOver` to FALSE to stop process
  while (loopOver) {
    
    # Begin main loop if analysis is to be carried out
    if (loopOverToday) {
      
      # Get competition info
      competitions <- getCompetitions()
      
      # Go through each competition and store results in
      # redis by calling the controller
      if (!is.null(competitions)) {
        for (i in 1:length(competitions)) {
          
          # Check if the current date is less than the last analysed date...
          competitionID <- competitions$id[i]
          lastDate <- redisConnection$GET(key = paste0(competitionID, ':lastActiveDate'))
          if (is.null(lastDate)) {
            lastDate <- 0
          } else {
            lastDate <- as.integer(lastDate)
          }
          
          # Run the controller if the current date being analysed hasnt been surpassed
          if (lastDate < currentDate) {
            daemonController(redisConnection = redisConnection,
                             competitionID = competitionID,
                             currentDate = as.Date(currentDate, origin = '1970-01-01'))
          }
        }
      } else {
        print(paste0(Sys.time(), ' : No competitions found. Aborting.'))
        loopOver <- FALSE
      }
      loopOverToday <- FALSE
    }
 
    if (currentDate > classificationStartTime) {
      timeToClassify <- TRUE
    }

    # Always look a few days in the future to get fixtures
    # Only active when the daemon has caught up with today
    if (currentDate == as.integer(Sys.Date) && timeToClassify) {
      
      # Calculate new query dates...
      datePOS <- as.Date(currentDate, origin = '1970-01-01') + 1
      dateFrom <- formatDates(standardDateFormat = datePOS)
      dateTo <- formatDates(standardDateFormat = datePOS + 4)
      
      # Query in the future for matches
      classifyFixtures(redisConnection = redisConnection,
                       competitionID = competitionID,
                       dateFrom = dateFrom,
                       dateTo = dateTo)
    }

    # If it is a new day then begin analysis over again!
    Sys.sleep(sleepTime)
    if (currentDate != as.integer(Sys.Date()) {
      loopOverToday <- TRUE
      currentDate <- currentDate + 1
    } else if (currentDate == as.integer(Sys.Date())) {
      currentDate <- as.integer(Sys.Date())
    }
  }
}