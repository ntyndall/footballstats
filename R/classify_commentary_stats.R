#' @title classify_commentary_stats
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


classify_commentary_stats <- function(commentary, returnItems) {
  vals <- sapply(1:length(commentary), function(j) {
    return(classify_commentary_from_redis(keyName = commentary[j],
                                          returnItems = returnItems))
  })
  
  if (length(returnItems) == 1) {
    return(sum(vals)/as.double(length(vals)))
  } else {
    columns <- ncol(vals)
    return(sapply(1:nrow(vals), function(k) {
      sum(vals[k, 1:columns])/as.double(columns)
    }))
  }
}
