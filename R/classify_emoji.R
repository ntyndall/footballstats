#' @title Emoji's for Slack
#'
#' @description A hashmap of emoji's that are printed to slack
#'  i.e. \code{:premier-ars:} and the corresponding teamID.
#'
#' @details In order to add a new icon, it must be resized that
#'  slack accepts it. Give it a name, and assign it below here
#'  with whichever competition.
#'
#' @return A hashmap of key-value pairs of icon name to be
#'  printed to slack and the corresponding teamID.
#'
#' @export

classify_emoji <- function() { # nocov start

  # England details
  englandOne <- c(
    'ars', 'eve', 'new', 'hud', 'bou', 'mun', 'sto',
    'wes', 'ham', 'che', 'bri', 'bur', 'liv', 'tot',
    'lei', 'cry', 'sou', 'wat', 'swa', 'mci', 'ast'
  )
  englandOne <- paste0(':premier-', englandOne, ':')

  # England IDs
  englandOneIds <- c(
    9002, 9158, 9287, 9220, 9053, 9260, 9378, 9426,
    9427, 9092, 9065, 9072, 9249, 9406, 9240, 9127,
    9363, 9423, 9387, 9259, 9008
  )

  # Championship details
  englandTwo <- c(
    'not', 'sun', 'bur', 'pre', 'ful', 'bri', 'ips',
    'que', 'wol', 'bol', 'car', 'mil', 'bir', 'der',
    'hul', 'rea', 'mid', 'bre', 'she', 'lee', 'bar',
    'nor', 'sheu', 'rot', 'wig', 'bla'
  )
  englandTwo <- paste0(':england-', englandTwo, ':')

  # Championship IDs
  englandTwoIds <- c(
    9297, 9384, 9074, 9317, 9175, 9066, 9227, 9318,
    9446, 9048, 9083, 9276, 9039, 9133, 9221, 9324,
    9274, 9059, 9349, 9238, 9021, 9296, 9348, 9333,
    9434, 9044
  )

  # Belgium details
  belgium <- c(
    'and', 'bru', 'cha', 'gen', 'kor', 'krc', 'lok',
    'mec', 'oos', 'sin', 'sta', 'waa', 'zul'
  )
  belgium <- paste0(':belgium-', belgium, ':')

  # Belgium IDs
  belgiumIds <- c(
    6740, 6752, 6751, 6776, 6813, 6775, 6829, 6833,
    6844, 6882, 6894, 6856, 6930
  )

  # Greece details
  greece <- c(
    'aek', 'ast', 'atr', 'ker', 'lev', 'oly', 'pane',
    'pani', 'pao', 'pas', 'lam', 'pla', 'apo', 'xan',
    'lar'
  )
  greece <- paste0(':greece-', greece, ':')

  greeceIds <- c(
    10747, 10772, 10775, 10833, 10817, 10829, 10843,
    10846, 10852, 10854, 10814, 10860, 10766, 10881,
    10815
  )

  # Portugal details
  portugal <-  c(
    'bel', 'ben', 'bfc', 'bra', 'mar', 'pac', 'por',
    'rio', 'vit', 'vitset', 'est', 'des', 'ton', 'pmo',
    'cha', 'spo', 'fei', 'mor', 'cdn', 'san'
  )
  portugal <- paste0(':portugal-', portugal, ':')

  portugalIds <- c(
    14265, 14267, 14270, 14446, 14378, 14399, 14408,
    14414, 14473, 14474, 14317, 14310, 14294, 14407,
    14311, 14448, 14339, 14387, 14389, 14417
  )

  # Germany details
  germany <- c(
    'baymun', 'ham', 'her', 'hof', 'main', 'wol',
    'sch', 'fre', 'bormc', 'kol', 'aug', 'einfra',
    'bordor', 'baylev', 'wer', 'vfb', 'han', 'lei',
    'for', 'nur'
  )
  germany <- paste0(':germany-', germany, ':')

  # Germany IDs
  germanyIds <- c(
    10285, 10419, 10437, 10442, 10388, 10653, 10576,
    10382, 10307, 10476, 10269, 10347, 10303, 10281,
    10677, 10646, 10423, 10552, 10377, 10372
  )

  # Russia details
  russia <- c(
    'spa', 'din', 'lok', 'csk', 'ars', 'zen', 'ufa',
    'kra', 'amk', 'ura', 'rub', 'ros', 'ore', 'kry',
    'yen'
  )
  russia <- paste0(':russia-', russia, ':')

  # Russia IDs
  russiaIds <- c(
    14921, 14803, 14871, 14796, 14787, 14950, 14792,
    14827, 14784, 14939, 14896, 14837, 14852, 14862,
    14879
  )

  # Turkey details
  turkey <- c(
    'ist', 'akh', 'gen', 'goz', 'ala', 'yen', 'tra',
    'bes', 'gal', 'kas', 'osm', 'kar', 'bur', 'siv',
    'fen', 'ant', 'kay', 'kon', 'erz', 'riz', 'ank'
  )
  turkey <- paste0(':turkey-', turkey, ':')

  # Turkey IDs
  turkeyIds <-  c(
    17047, 16971, 17035, 17038, 16973, 17072, 17106,
    16994, 17029, 17057, 16980, 17053, 17002, 17097,
    17027, 16981, 17060, 17068, 19918, 17006, 16979
  )

  # Italy details
  italy <- c(
    'asr', 'ata', 'ben', 'bol', 'cag', 'chi', 'cro', 'fio',
    'gen', 'hel', 'int', 'juv', 'laz', 'mil', 'nap',
    'sam', 'spa', 'tor', 'udi', 'uss', 'fro', 'par',
    'emp'
  )
  italy <- paste0(':italy-', italy, ':')

  # France IDs
  italyIds <- c(
    11998, 11811, 11821, 11822, 11830, 11850, 11861, 11894,
    11903, 11914, 11917, 11922, 11925, 11938, 11947,
    12005, 12026, 12046, 12051, 12013, 11898, 11959,
    11867
  )

  # France details
  france <- c(
    'ami', 'ang', 'bor', 'cae', 'dij', 'gui',
    'lil', 'lyo', 'mac', 'mar', 'met', 'mon',
    'nic', 'nnt', 'psg', 'ren', 'sai', 'str', 'tou',
    'tro', 'nim', 'sta'
  )
  france <- paste0(':france-', france, ':')

  # France IDs
  franceIds <- c(
    9826, 9831, 9875, 9883, 9909, 9973,
    10004, 10040, 10020, 10042, 10018, 10024,
    10033, 10031, 10061, 10122, 10085, 10124, 10134,
    10140, 10034, 10073
  )

  # Spain details
  spain <- c(
    'atb', 'atm', 'bar', 'cel', 'dea', 'del',
    'eib', 'esp', 'get', 'gir', 'leg', 'lev',
    'mal', 'bet', 'mad', 'soc', 'sev', 'las',
    'val', 'vil', 'ray', 'hue', 'vll'
  )
  spain <- paste0(':spain-', spain, ':')

  # France IDs
  spainIds <- c(
    15679, 15692, 15702, 15934, 15997, 15999,
    16006, 16009, 16017, 16021, 15833, 16043,
    16050, 16107, 16110, 16117, 16175, 16040,
    16261, 16270, 16098, 16030, 16123
  )

  # Set up keynames as the IDs
  keyNames <- c(
    englandOneIds, englandTwoIds, belgiumIds,
    greeceIds, portugalIds, germanyIds,
    russiaIds, turkeyIds, franceIds,
    italyIds, spainIds
  ) %>%
    as.integer

  # Create the hash
  myHash <- keyNames %>%
    hashmap::hashmap(
      values = c(
        englandOne, englandTwo, belgium, greece,
        portugal, germany, russia, turkey, france,
        italy, spain
      )
    )

  # Return hash back
  return(myHash)
} # nocov end
