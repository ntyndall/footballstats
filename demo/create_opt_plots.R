library(ggplot2)
library(magrittr)
library(gridExtra)


# Load up the CSV
my.results <- getwd() %>%
  paste0("/results_optimization/results.csv") %>%
  read.csv2(
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

# Convert to numeric
for (i in 7:(my.results %>% ncol)) my.results[ , i] %<>% as.numeric

# Function for ggplot
create_gg <- function(dat.set, xval, yval) {
  return(
    ggplot2::ggplot(
      data = dat.set,
      mapping = ggplot2::aes(
        x = dat.set[[xval]] %>% as.factor,
        y = dat.set[[yval]],
        fill = dat.set[[xval]] %>% as.factor
      )
    ) %>%
      `+`(ggplot2::geom_boxplot()) %>%
      `+`(ggplot2::geom_jitter(width = 0.2)) %>%
      `+`(footballstats::plot_theme()) %>%
      `+`(ggplot2::scale_fill_discrete(xval %>% stringr::str_to_title())) %>%
      `+`(ggplot2::xlab(xval %>% stringr::str_to_title())) %>%
      `+`(ggplot2::ylab(yval %>% stringr::str_to_title()))
  )
}

myFactors <- c("day", "gridPoints", "gridBoundary", "decay", "totalPercentage")

for (i in 1:(myFactors %>% length)) {

  # Create the individual plots
  res <- lapply(
    X = c("accuracy", "profit"),
    FUN = function(x) my.results %>% create_gg(xval = myFactors[i], yval = x)
  )

  # Set up grob table
  gridPlots <- gridExtra::grid.arrange(
    res[[1]],
    res[[2]],
    nrow = 2
  )

  # Save the image to /images/
  ggplot2::ggsave(
    filename = getwd() %>% paste0("/images/", myFactors[i], ".png"),
    gridPlots
  )
}
