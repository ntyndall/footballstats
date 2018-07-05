#' @title Create Plot
#'
#' @export


create_plot <- function(total.results, day, gridPoints, gridBoundary, decayFactor, totalPer) {

  # Get all unique feature names
  singlePlots <- total.results %>%
    names %>%
    setdiff("res")

  # Set up stock file name for parameters
  toplot <- singlePlots %>% length
  fileName <- paste0(
    "day", day, "-gridPoints", gridPoints, "-gridBoundary", gridBoundary,
    "-decayFactor", decayFactor, "-totalPer", totalPer
  )

  # Check that all directories exist
  dirNames <- singlePlots %>%
    strsplit(split = '[.]') %>%
    purrr::map(function(x) x %>% paste(collapse = '_')) %>%
    purrr::flatten_chr()

  # Which directories don't exist
  dirExists <- dirNames %>%
    dir.exists %>%
    `!`()

  # If any then create them
  if (dirExists %>% any) {
    created <- capture.output(
      getwd() %>% paste0(
        "/param_optimization/",
        dirNames %>% `[`(dirExists %>% which)
      ) %>%
        sapply(dir.create)
    )
  }

  # Initialise data frame + get result truth vector
  plot.metrics <- data.frame(stringsAsFactors = FALSE)
  resTruth <- list(
    win = total.results$res %>% `==`('W') %>% which,
    lose = total.results$res %>% `==`('L') %>% which,
    draw = total.results$res %>% `==`('D') %>% which
  )

  # Plot each detail
  for (i in 1:toplot) {

    # Define the plot title
    plotTitle <- paste0(
      "day : ", day, " | gridP : ", gridPoints,
      " | gridB : ", gridBoundary,
      " | decay : ", decayFactor, " | total % : ", totalPer
    )

    # Create actual density ggplot
    g <- ggplot2::ggplot(data = total.results, mapping = ggplot2::aes(x = get(singlePlots[i]), fill = res)) %>%
      `+`(ggplot2::geom_density(alpha=.3)) %>%
      `+`(ggplot2::ggtitle(plotTitle)) %>%
      `+`(ggplot2::xlab(label = singlePlots[i])) %>%
      `+`(ggplot2::scale_fill_manual(values = c("blue", "red", "green"))) %>%
      `+`(footballstats::plot_theme())

    # Save using the correct details
    ggplot2::ggsave(
      filename = getwd() %>% paste0("/param_optimization/", dirNames[i], "/", fileName, ".png"),
      plot = g
    )

    # Define current values of feature
    vals <- total.results[[singlePlots[i]]]

    # Create actual metrics from the features
    plot.metrics %<>% rbind(
      data.frame(
        day = day,
        gridPoints = gridPoints,
        gridBoundary = gridBoundary,
        decayFactor = decayFactor,
        totalPer = totalPer,
        featureName = singlePlots[i],
        winMu = vals[resTruth$win] %>% mean,
        winSD = vals[resTruth$win] %>% stats::sd(),
        loseMu = vals[resTruth$lose] %>% mean,
        loseSD = vals[resTruth$lose] %>% stats::sd(),
        drawMu = vals[resTruth$draw] %>% mean,
        drawSD = vals[resTruth$draw] %>% stats::sd(),
        stringsAsFactors = FALSE
      )
    )
  }

  # Return the data frame back
  return(plot.metrics)
}
