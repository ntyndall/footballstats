#' 
#' @details Test getNextID()
#'


# Begin testing  
test_that("first test", {
  
  IDLookup <- "match"  
  getNextID(IDLookup = IDLookup,
            IDList = c("match"))
  
  result <- rredis::redisGet(key = paste0("next:", IDLookup, ":id"))
  result <- as.integer(result)

  expect_that( 1, is_a("numeric"))
  expect_that( result, equals(1) )
  
})
