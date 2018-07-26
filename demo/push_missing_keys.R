# This is a script due to misaligned competitions not being
# saved on production...

# These are the competitions to look at
updateComps <- c('1399', '1457')

# Connect to local DB
footballstats::redis_con()

# Port Forward and connect to server DB
# ssh -L 3333:localhost:6379 root@{IP}
myCon <- redux::hiredis(
  host = 'localhost',
  port = '3333',
  db = 1
)

# Loop over the competitions
for (i in 1:(updateComps %>% length)) {

  # Print statement
  cat(' ## Looking at ', i, '/', updateComps %>% length, '\n\n')

  # Key pattern
  kPat <- paste0("*:", updateComps[i], ":*")

  # Get all the Keys locally -
  localKeys <- kPat %>%
    rredis::redisKeys()

  # Get all the keys on digital ocean -
  extKeys <- kPat %>%
    myCon$KEYS() %>%
    purrr::flatten_chr()

  # Any actually not on the server?
  mismatch <- localKeys %>% setdiff(extKeys)
  if (mismatch %>% length %>% `==`(0)) next

  # Get ALL the key types
  typesOfKeys <- mismatch %>%
    strsplit(split = ':') %>%
    purrr::map(1) %>%
    purrr::flatten_chr()

  # Get the unique key starters
  uniqueKeys <- typesOfKeys %>% unique

  # Alright - some pre-emptive work here
  # [1] "cmp"            "cme"            "csm"            "cwt_l"          "ctp"            "cmt_commentary"
  # [7] "cw_pl"          "ct_basic"       "csdm_pred"      "ct_stats"       "comp"           "c_startDate"
  # I know these are all the types of keys, now I just need to map the type accuractly

  # Set up progress bar
  pBar <- utils::txtProgressBar(
    min = 0,
    max = mismatch %>% length,
    style = 3
  )

  for (j in 1:(mismatch %>% length)) {

    # Update progress bar
    utils::setTxtProgressBar(
      pb = pBar,
      value = j
    )

    # Current type..
    cT <- typesOfKeys[j]

    # Current name...
    cN <- mismatch[j]

    # Build the rules for redis types here..
    # Do a get from local - push to ext. host
    if (cT %in% c("c_startDate")) { # REDIS GET/SET
      # Get from local...
      items <- cN %>%
        rredis::redisGet()

      # And push to host...
      cN %>%
        myCon$SET(value = items %>% as.character)
    } else { # REDIS HASH
      # Get from local...
      items <- cN %>%
        rredis::redisHGetAll()

      # And push to host...
      cN %>% myCon$HMSET(
        field = items %>% names,
        value = items %>% as.character
        )
    }
  }
}
