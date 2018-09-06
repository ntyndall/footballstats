#' @title Missing Commentaries
#'
#' @export


missing_commentaries <- function(KEYS) {

  # First get all the basic match stats
  allCSMs <- "csm:*" %>%
    KEYS$RED$KEYS() %>%
    purrr::flatten_chr()

  # Get all seasons as a vector
  allSeasons <- allCSMs %>%
    strsplit(split = ":") %>%
    purrr::map(3) %>%
    purrr::flatten_chr()

  # Get unique seasons
  uniqueSeasons <- allSeasons %>%
    unique %>%
    sort

  # Split them up
  splitCSM <- lapply(
    X = uniqueSeasons,
    FUN = function(x) allCSMs %>% `[`(x %>% `==`(allSeasons))
  )

  # Now for each season, get all the commentary keys
  splitCSMT <- lapply(
    X = uniqueSeasons,
    FUN = function(x) paste0("csmt_*:", x, "*") %>% KEYS$RED$KEYS() %>% purrr::flatten_chr()
  )

  # Loop over all seasons (Just for reporting purposes)
  for (i in 1:(uniqueSeasons %>% length)) {

    # Just skip this for now.
    if (uniqueSeasons[i] %>% `==`("2017")) next

    # Get all match IDs
    matchIDs <- splitCSM[[i]] %>%
      strsplit(split = ":") %>%
      purrr::map(4) %>%
      purrr::flatten_chr()

    # Get commentary IDs
    comIDs <- splitCSMT[[i]] %>%
      strsplit(split = ":") %>%
      purrr::map(4) %>%
      purrr::flatten_chr()

    matched <- lapply(
      X = matchIDs,
      FUN = function(x) x %>% `==`(comIDs) %>% sum %>% `!=`(2)
    ) %>%
      purrr::flatten_lgl()

    # If any aren't matched then report on them (and also get the details)
    if (matched %>% any) {

      # Log to screen...
      cat(" ~*~* Missing Commentaries *~*~ \n\n")

      # Only get the CSM that need reported on
      missingComm <- splitCSM[[i]] %>%
        `[`(matched)

      # Sort them
      missingComm %<>% `[`(
        missingComm %>%
          strsplit(split = ":") %>%
          purrr::map(2) %>%
          purrr::flatten_chr() %>%
          as.integer %>%
          order
        )

      results <- KEYS$RED$pipeline(
        .commands = lapply(
          X = missingComm,
          FUN = function(x) {
            x %>% KEYS$PIPE$HMGET(
              field = c("localteam_name", "visitorteam_name", "formatted_date")
            )
          }
        )
      )

      # Now print out the results!
      cat(
        "",
        paste0(
          results %>% purrr::map(1) %>% purrr::flatten_chr(),
          " vs. ",
          results %>% purrr::map(2) %>% purrr::flatten_chr(),
          " | Date = ",
          results %>% purrr::map(3) %>% purrr::flatten_chr() %>% as.Date(format = "%d.%m.%Y"),
          "\n"
        )
      )
    }
  }
}
