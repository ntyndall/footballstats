#' @title Recreate Match Data
#'
#' @description A function that provided with a competitionID and season start year,
#'  can auto generate and order by date all the basic match information
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}"{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @return matchData. A data frame containing all the matches in a particular season.
#'
#' @export


recreate_matchdata <- function(KEYS, matchLimit = 10000) {
  allMatches <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, '*') %>% rredis::redisKeys()
  matchData <- data.frame(stringsAsFactors = FALSE)
  if (!is.null(allMatches)) {
    for (i in 1:length(allMatches)) {
      singleMatch <- rredis::redisHGetAll(
        key = allMatches[i]
      )

      matchID <- data.frame(
        singleMatch %>% as.character %>% t,
        stringsAsFactors = FALSE
      )
      matchIDName <- singleMatch[c(TRUE, FALSE)]
      names(matchID) <- names(singleMatch)
      matchData %<>% rbind(matchID)
    }
  }

  # Only look back at the previous `x` matches.
  if (`<`(matchData %>% nrow(), 1)) {
    print(paste0(Sys.time(), ' : No match data found for the providing input parameters.'))
  } else {
    # Re-order the dataframe by date.
    matchData %<>% footballstats::order_matchdata(
      limit = matchLimit
    )
  }
  return(matchData)
}

#' @title Order Match Dataset
#' @export


order_matchdata <- function(matchData, limit = 5000) {
  matchData$formatted_date %<>% as.Date('%d.%m.%Y')
  matchData <- matchData[matchData$formatted_date %>% order, ]
  limit %<>% min(matchData %>% nrow)
  return(matchData[1:limit, ])
}

#' @title Commentary From Redis
#' @export


commentary_from_redis <- function(keyName, returnItems) {
  # Get all commentary items
  results <- keyName %>% rredis::redisHMGet(
    fields = returnItems
  )
  hashNames <- results %>% names
  hashLen <- hashNames %>% length

  # Make sure something exists in the requested fields
  vec <- c()
  for (j in 1:hashLen) {
    single <- results[[hashNames[j]]]
    vec %<>% c(
      if (hashNames[j] %>% `==`('possesiontime')) {
        single %>% gsub(
          pattern = "%",
          replacement = ""
        ) %>% as.double
      } else if (single %>% is.null) {
        NA
      } else if (single %>% is.na) {
        NA
      } else {
        if (single %>% `==`('')) 0 else single %>% as.double
      }
    )
  }

  # Make sure the items returned is the same length as requested
  return(
    if (vec %>% stats::na.omit() %>% as.double %>% length %>% `==`(hashLen)) vec else NULL
  )
}

#' @title Scale SVM Data
#' @export


scale_data <- function(mDat, dataScales) {

  scaled.data <- mDat[ , 1:dataScales$cols] %>% scale(
    center = dataScales$sMin,
    scale = dataScales$sMax - dataScales$sMin
  ) %>% as.data.frame

  if ('res' %in% (mDat %>% colnames)) scaled.data %<>% cbind(res = mDat$res)
  scaled.data %>% return()
}

#' @title Scale SVM Data
#'
#' @details We subtract 1 from the data frame as the data set
#'  MUST always have a leading column that contains labelled
#'  match data with 'W / D / L' etc. Also, one feature is not
#'  permitted, so mDat must therefore have 3 or more columns.
#'
#' @param mDat A data frame with multiple columns, each one
#'  containing a feature, and a final row with labelled results
#'  i.e. 'W / D / L'.
#'
#' @return A list value with the following names :
#' \itemize{
#'  \item{sMax : maximum values per feature}
#'  \item{sMin : minimum values per feature}
#'  \item{cols : an integer defining the number of columns
#'   of sMax and sMin}
#'  }
#'
#' @export


get_scales <- function(mDat) {
  nc <- `-`(mDat %>% ncol, 1)
  maxs <- apply(mDat[ , 1:nc], 2, max)
  mins <- apply(mDat[ , 1:nc], 2, min)
  list(sMax = maxs, sMin = mins, cols = nc) %>% return()
}
