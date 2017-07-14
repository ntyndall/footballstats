#' @title Get Next ID
#'
#' @description A function that takes a character string with the type
#'  of action required and generates a new ID number for internal purposes.
#'  
#' @details All data is stored in relevant redis hash maps. A generic ID from
#'  the API is provided, then mapped to a value starting from 1, and counting
#'  upwards for each action, e.g. match / team / player / competition.
#'     ->   next:{IDLookup}:id
#'
#' @import maggrittr
#' 
#' @param IDLookup A character string containing an action, e.g. match / team 
#'  which requires a new ID value for storing in redis.
#'  
#' @return IDValue An integer value representing the next ID per action is returned
#'  by incrementing a simple key value pair in Redis
#'

getNextID <- function(IDLookup = NULL, IDList = IDList) {
  if (IDLookup %in% IDList) { 
    keyStr <- paste0("next:", IDLookup, ":id") 
  } else { 
    IDLookup <- NULL 
  }
  
  if (is.null(IDLookup)) { 
    return(paste0("Error : Return one of - ", paste0(IDList, collapse = ", ")))
  } else {
    return(as.integer(rredis::redisIncr(key = keyStr)))
  }
}

