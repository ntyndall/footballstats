



lookUpSlackEmoji <- function() {
  emojiNames <- c('ars', 'eve', 'new', 'hud', 'bou', 'mun', 'sto', 'wes', 'ham', 'che', 
                  'bri', 'bur', 'liv', 'tot', 'lei', 'cry', 'sou', 'wat', 'swa', 'mci')
  myHash <- hashmap::hashmap(keys = c(9002, 9158, 9287, 9220, 9053, 9260, 9378, 9426, 9427, 9092,
                                      9065, 9072, 9249, 9406, 9240, 9127, 9363, 9423, 9387, 9259), 
                             values = paste0(':premier-', emojiNames, ':'))
  return(myHash)
}
