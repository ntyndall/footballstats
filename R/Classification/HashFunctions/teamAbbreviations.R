



teamAbbreviations <- function() {
  teamNames <- c('ARS', 'BHA', 'BOU', 'BUR', 'CHE', 'CRY', 'EVE', 'HUD', 'LEI', 'LIV', 
                 'MCI', 'MUN', 'NEW', 'SOU', 'STK', 'SWA', 'TOT', 'WAT', 'WBA', 'WHU')
  myHash <- hashmap::hashmap(keys = c(9002, 9065, 9053, 9072, 9092, 9127, 9158, 9220, 9240, 9249,
                                      9259, 9260, 9287, 9363, 9378, 9387, 9406, 9423, 9426, 9427), 
                             values = teamNames)
  return(myHash)
}
