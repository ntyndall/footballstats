#' @title Recreate Match Data
#'
#' @description A function that provided with a competitionID and season start year,
#'  can auto generate and order by date all the basic match information
#'
#' @details A search for all matches in a particular subset is made in Redis, a data frame
#'  is then constructed to rebuild the original API query and ordered by date.
#'
#' @param competitionID An integer containing the competitionID that the
#'  teams and match information belong to.
#' @param seasonStarting An integer defining the lower year for details on a season.
#' @param redisData An environment that defines the redis configuration where data is
#'  to be searched for.
#'
#' @return matchData. A data frame containing all the matches in a particular season.
#'
#' @export


recreate_matchdata <- function(competitionID, seasonStarting, matchLimit) {
  allMatches <- rredis::redisKeys(
    pattern = paste0('csm:', competitionID, ':', seasonStarting, '*'))
  matchData <- data.frame(stringsAsFactors = FALSE)
  if (!is.null(allMatches)) {
    for (i in 1:length(allMatches)) {
      singleMatch <- rredis::redisHGetAll(
        key = allMatches[i])

      matchID <- data.frame(singleMatch %>% as.character() %>% t(),
                            stringsAsFactors = FALSE)
      matchIDName <- singleMatch[c(TRUE, FALSE)]
      names(matchID) <- names(singleMatch)
      matchData <- rbind(matchData, matchID)
    }
  }

  # Only look back at the previous `x` matches.
  if (`<`(matchData %>% nrow(), 1)) {
    print(paste0(Sys.time(), ' : No match data found for the providing input parameters.'))
  } else {
    # Re-order the dataframe by date.
    matchData <- footballstats::order_matchdata(
      matchData = matchData,
      limit = matchLimit)
  }
  return(matchData)
}

#' @title Order Match Dataset
#' @export


order_matchdata <- function(matchData, limit = 5000) {
  matchData$formatted_date <- matchData$formatted_date %>% as.Date('%d.%m.%Y')
  matchData <- matchData[matchData$formatted_date %>% order(), ]
  limit <- min(limit, matchData %>% nrow())
  return(matchData[1:limit, ])
}

#'
#' @export

available_commentaries <- function(competitionID = 'all', includeNames = 'all') {

  # Single competitionID or all
  allowedComps <- footballstats::allowed_comps()

  commentaryKeys <- c()
  for (i in 1:(allowedComps %>% length)) {
    commentaryKeys %<>% c(paste0('cmt_commentary:', allowedComps[i], '*') %>%
      rredis::redisKeys() %>% as.character)
  }

  allAvailable <- c()
  getAll <- if (`==`(includeNames %>% length, 1)) TRUE else FALSE
   for (x in 1:length(commentaryKeys)) {
    results <- commentaryKeys[x] %>% rredis::redisHGetAll()
    cNames <- results %>% names
    cValues <- results %>% as.character
    empties <- cValues == ""


    # Default to all
    if (`==`(x, 1) && getAll) includeNames <- cNames %>% subset(cNames != 'table_id')

    # Remove any empty string fields
    cNames <- if (empties %>% any) cNames[-which(empties)] else cNames

    # Remove any predefined variables that should never be used
    intersection <- intersect(cNames, includeNames)

    if (identical(intersection, character(0))) stop('A complete set of required names does not exist.')
    allAvailable <- if (x == 1) includeNames else intersect(intersection, allAvailable)
  }
  return(allAvailable)
}

#' @title commentary_stats
#'
#' @description A function that takes the commentary values stored in redis and
#'  calculates an average value for the list of variables in the key for that team.
#'
#' @param commentary A character vector of redis keys that hold a teams match
#'  commentary.
#' @param returnItems A vector of character values that hold the names of
#'  fields to be returned for the commentary statistics.
#'
#' @return A average statistics for a particular team.
#'
#' @export


commentary_stats <- function(commentary, returnItems) {
  vals <- sapply(1:length(commentary), function(j) {
    return(footballstats::commentary_from_redis(
      keyName = commentary[j],
      returnItems = returnItems))
  })

  if (`==`(returnItems %>% length, 1)) {
    return(`/`(vals %>% sum, vals %>% length %>% as.double))
  } else {
    columns <- vals %>% ncol
    return(sapply(1:(vals %>% nrow), function(k) {
      `/`(vals[k, 1:columns] %>% sum, columns %>% as.double)
    }))
  }
}

#'
#' @export


commentary_from_redis <- function(keyName, returnItems) {
  results <- rredis::redisHMGet(
    key = keyName,
    fields = returnItems)

  if ("possesiontime" %in% returnItems) {
    results$possesiontime <- gsub(
      pattern = "%",
      replacement = "",
      x = results$possesiontime)
  }

  results %>% as.double %>% return()
}

#' @title Scale SVM Data
#' @export


scale_data <- function(mDat, dataScales) {

  scaled.data <- mDat[ , 1:dataScales$cols] %>% scale(
    center = dataScales$sMin,
    scale = dataScales$sMax - dataScales$sMin) %>%
    as.data.frame()

  if ('res' %in% (mDat %>% colnames)) scaled.data %<>% cbind(res = mDat$res)
  scaled.data %>% return()
}

#' @title Scale SVM Data
#' @export


get_scales <- function(mDat) {
  nc <- `-`(mDat %>% ncol, 1)
  maxs <- apply(mDat[ , 1:nc], 2, max)
  mins <- apply(mDat[ , 1:nc], 2, min)
  list(sMax = maxs, sMin = mins, cols = nc) %>% return()
}
