#' @title Recreate Match Data
#'
#' @description A function that can auto generate and order by date all
#'  the basic match information if it exists in redis.
#'
#' @details Redis Keys used;
#'   \itemize{
#'     \item{\strong{[HASH]} :: \code{csm:{comp_id}:{season}:{match_id}}}
#'   }
#'
#' @param KEYS A list containing options such as testing / prediction /
#'  important variables and information. Also contains API information.
#'
#' @return matchData. A data frame containing all the matches in a particular season.
#'
#' @export


recreate_matchdata <- function(KEYS) {

  # Get all the redis match keys
  allMatches <- paste0('csm:', KEYS$COMP, ':', KEYS$SEASON, '*') %>%
    KEYS$RED$KEYS()

  # If keys exist then create a data frame
  if (allMatches %>% length %>% `>`(0)) {
    matchRes <- KEYS$RED$pipeline(
      .commands = lapply(
        X = allMatches %>% purrr::flatten_chr(),
        FUN = function(x) x %>% KEYS$PIPE$HGETALL()
      )
    ) %>%
      lapply(footballstats::create_hash)

    matchData <- lapply(
      X = 1:(matchRes %>% length),
      FUN = function(x) {
        matchRes[[x]] %>% data.frame(stringsAsFactors = FALSE)
      }
    ) %>%
      purrr::reduce(rbind) %>%
      footballstats::order_matchdata()
  } else {
    print(paste0(Sys.time(), ' : No match data found for the providing input parameters.'))
    matchData <- data.frame()
  }

  return(matchData)
}

#' @title Order Match Dataset
#'
#' @description A function that takes an arbitrary data frame
#'  consisting of match data and orders it by date in ascending
#'  order.
#'
#' @param matchData A data frame that contains rows of single matches
#'  that have been played between two teams.
#'
#' @return A data frame that has been ordered by date.
#'
#' @export


order_matchdata <- function(matchData) {
  matchData$formatted_date %<>% as.Date('%d.%m.%Y')
  matchData <- matchData[matchData$formatted_date %>% order(matchData$id), ]
  return(matchData)
}

#' @title Commentary From Redis
#'
#' @description A function that retrieves the basic commentary
#'  information from redis.
#'
#' @param keyName A character string that defines the redis key
#'  where the commentary is stored.
#' @param returnItems A character vector defining the names of the
#'  fields in the commentary key in redis to be retrieved.
#'
#' @return A vector of integers that correspond to the \code{returnItem}
#'  values requested.
#'
#' @export


commentary_from_redis <- function(KEYS, keyName, returnItems) {
  # Get all commentary items
  results <- keyName %>%
    KEYS$RED$HMGET(
      field = returnItems
    )
  names(results) <- returnItems

  # Replace possesiontime here
  if ("possesiontime" %in% returnItems) {
    results$possesiontime %<>% gsub(
      pattern = "%",
      replacement = ""
    )
  }

  # Filter out items that don't exist
   nonNull <- results %>%
     lapply(length) %>%
     purrr::flatten_int() %>%
     as.logical

  # Vectorise and convert to doubles where  necessarcy
  vec <- sapply(
    X = 1:(results %>% length),
    FUN = function(x) {
      it <- results[[x]]
      if (nonNull[x]) {
        if (it == "") 0 else it %>% as.double
      } else {
        NA
      }
    }
  )

  # Make sure the items returned is the same length as requested
  return(if (vec %>% anyNA) NULL else vec)
}

#' @title Scale Data
#'
#' @description A function that takes a data set and scals it based
#'  on the scaling parameters that have already been calculated elsewhere.
#'
#' @param mDat A data frame that defines the data set used to build the model
#'  which is currently UNSCALED.
#' @param dataScales A list which contains multiple information, including \code{sMax}
#'  and \code{sMin} which define the bounds of the \code{dataScale$commentaries} values.
#'
#' @return A data frame the same size as \code{mDat}, which has now been scaled.
#'
#' @export


scale_data <- function(mDat, dataScales) {

  scaled.data <- mDat[ , 1:dataScales$cols] %>% scale(
    center = dataScales$sMin,
    scale = dataScales$sMax - dataScales$sMin
  ) %>% as.data.frame

  if ('res' %in% (mDat %>% colnames)) scaled.data %<>% cbind(res = mDat$res)
  return(scaled.data)
}
