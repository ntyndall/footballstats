



lookUpSlackEmoji <- function() {
  
  # England details
  englandOne <- paste0(':premier-', 
                       c('ars', 'eve', 'new', 'hud', 'bou', 'mun', 'sto', 
                         'wes', 'ham', 'che', 'bri', 'bur', 'liv', 'tot', 
                         'lei', 'cry', 'sou', 'wat', 'swa', 'mci', 'ast'),
                         ':')
  englandOneIds <- c(9002, 9158, 9287, 9220, 9053, 9260, 9378, 9426, 9427, 9092,
                     9065, 9072, 9249, 9406, 9240, 9127, 9363, 9423, 9387, 9259,
                     9008)
  
  englandTwo <- paste0(':england-', 
                       c('not', 'sun', 'bur', 'pre', 'ful', 'bri', 'ips', 
                         'que', 'wol', 'bol', 'car', 'mil', 'bir', 'der', 
                         'hul', 'rea', 'mid', 'bre', 'she', 'lee', 'bar', 
                         'nor', 'sheu'),
                       ':')
  englandTwoIds <- c(9297, 9384, 9074, 9317, 9175, 9066, 9227, 9318, 9446, 
                     9048, 9083, 9276, 9039, 9133, 9221, 9324, 9274, 9059, 9349, 
                     9238, 9021, 9296, 9348)

  # Belgium details
  belgium <- paste0(':belgium-', 
                    c('and', 'bru', 'cha', 'gen', 'kor', 'krc', 'lok', 'mec', 
                      'oos', 'sin', 'sta', 'waa', 'zul'), 
                    ':')
  belgiumIds <- c(6740, 6752, 6751, 6776, 6813, 6775, 6829, 6833, 
                  6844, 6882, 6894, 6856, 6930)
  
  # Greece details
  greece <- paste0(':greece-', 
                    c('aek', 'ast', 'atr', 'ker', 'lev', 'oly', 'pane', 
                      'pani', 'pao', 'pas', 'lam', 'pla', 'apo', 'xan',
                      'lar'), 
                    ':')
  greeceIds <- c(10747, 10772, 10775, 10833, 10817, 10829, 10843, 
                 10846, 10852, 10854, 10814, 10860, 10766, 10881,
                 10815)

  # Portugal details
  portugal <- paste0(':portugal-',
                     c('bel', 'ben', 'bfc', 'bra', 'mar', 'pac', 'por', 
                       'rio', 'vitgui', 'vitset', 'est'),
                     ':')
  portugalIds <- c(14265, 14267, 14270, 14446, 14378, 14399, 14408, 
                   14414, 14473, 14474, 14317)

  # Germany details
  germany <- paste0(':germany-',
                    c('baymun', 'ham', 'her', 'hof', 'main', 'wol', 'sch', 
                      'fre', 'bormc', 'kol', 'aug', 'einfra', 'bordor',
                      'baylev', 'wer', 'vfb', 'han', 'lei'),
                    ':')
  germanyIds <- c(10285, 10419, 10437, 10442, 10388, 10653, 10576, 
                  10382, 10307, 10476, 10269, 10347, 10303, 10281, 
                  10677, 10646, 10423, 10552)
  # Russia details
  russia <- paste0(':russia-',
                    c('spa', 'din', 'lok', 'csk', 'ars', 'zen', 'ufa', 
                      'kra', 'amk', 'ura', 'rub', 'ros'),
                    ':')
  russiaIds <- c(14921, 14803, 14871, 14796, 14787, 14950, 14792,
                 14827, 14784, 14939, 14896, 14837)

  # Turkey details
  turkey <- paste0(':turkey-',
                   c('ist', 'akh', 'gen', 'goz', 'ala', 'yen', 'tra', 
                     'bes', 'gal', 'kas', 'osm', 'kar', 'bur', 'siv',
                     'fen', 'ant', 'kay', 'kon'),
                   ':')
  turkeyIds <-  c(17047, 16971, 17035, 17038, 16973, 17072, 17106,
                  16994, 17029, 17057, 16980, 17053, 17002, 17097, 
                  17027, 16981, 17060, 17068)
  
  myHash <- hashmap::hashmap(keys = as.integer(c(englandOneIds, englandTwoIds, belgiumIds, greeceIds, portugalIds, germanyIds,
                                                 russiaIds, turkeyIds)),
                             values = c(englandOne, englandTwo, belgium, greece, portugal, germany, russia, turkey))
  return(myHash)
}
