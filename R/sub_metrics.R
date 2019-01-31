#' @title Sub Metrics
#'
#' @export


sub_metrics <- function(total.metrics, colNames, GRIDS, odds.frame = data.frame()) {
  odds.results <- total.results <- data.frame(stringsAsFactors = FALSE)
  allMatchIDs <- c()

  # Set up a progress bar here
  pb <- utils::txtProgressBar(
    min = 0,
    max = total.metrics %>% nrow,
    style = 3
  )

  # Now loop over all of total.metrics
  for (drow in 2:(total.metrics %>% nrow)) {

    # Update the progress bar
    utils::setTxtProgressBar(
      pb = pb,
      value = drow
    )

    current.row <- total.metrics[drow, ]
    smaller.metrics <- total.metrics[1:(drow - 1), ]

    # Subset smaller subset for logical matches
    haMatches <- list(
      hh = smaller.metrics[[colNames$localID]] %>% `==`(current.row[[colNames$localID]]),
      ah = smaller.metrics[[colNames$awayID]] %>% `==`(current.row[[colNames$localID]]),
      ha = smaller.metrics[[colNames$localID]] %>% `==`(current.row[[colNames$awayID]]),
      aa = smaller.metrics[[colNames$awayID]] %>% `==`(current.row[[colNames$awayID]])
    )

    # Number of rows of each type
    allSums <- haMatches %>% purrr::map(sum)

    if (allSums %>% purrr::map(function(x) x > DAYS[i]) %>% purrr::flatten_lgl() %>% all) {

      # Separating function
      sep_dat <- function(x, d, s) return(x[(s - d + 1):(x %>% nrow), ])

      # Get grid values here
      home.away.dat <- rbind(
        smaller.metrics %>% subset(haMatches$hh) %>% sep_dat(d = DAYS[i], s = allSums$hh),
        smaller.metrics %>% subset(haMatches$ah) %>% sep_dat(d = DAYS[i], s = allSums$ah),
        smaller.metrics %>% subset(haMatches$ha) %>% sep_dat(d = DAYS[i], s = allSums$ha),
        smaller.metrics %>% subset(haMatches$aa) %>% sep_dat(d = DAYS[i], s = allSums$aa)
      )

      # do calculations here
      i <- j <- k <- l <- m <- 1
      result.dat <- home.away.dat %>%
        footballstats::optimize_calculation(
          day = DAYS[i],
          gridPoints = GRID_PTS[j],
          gridBoundary= GRID_BOUND[k],
          decayFactor = DECAY[l],
          til = current.row$zzz.til,
          totalPer = TOTAL_PERC[m]
        )

      # Append positions on
      result.dat$`position.h` <- current.row$`position.h` %>% `/`(current.row$zzz.til)
      result.dat$`position.a` <- current.row$`position.a` %>% `/`(current.row$zzz.til)
      result.dat$res <- current.row$result
      total.results %<>% rbind(result.dat)
      allMatchIDs %<>% c(current.row$zzz.matchID)

      # Make sure there is a match, if not then set as NA
      matchingIndex <- current.row$zzz.matchID %>% `==`(odds.frame$matchID)
      odds.results %<>% rbind(
        if (matchingIndex %>% any) {
          odds.frame[matchingIndex %>% which, ]
        } else {
          data.frame(
            matchID = current.row$zzz.matchID,
            homewin = NA,
            draw = NA,
            awaywin = NA,
            stringsAsFactors = FALSE
          )
        }
      )
    } else {
      next
    }
  }

  # Match up matchIDs with odds frame
  new.odds <- odds.frame[allMatchIDs %>% match(odds.frame$matchID), ]

  # Remove NA's from matchIDs
  allMatchIDs %<>%
    `[`(total.results %>% stats::complete.cases())

  return(
    list(
      data = total.results,
      odds = new.odds,
      matchIDs = allMatchIDs
    )
  )
}
