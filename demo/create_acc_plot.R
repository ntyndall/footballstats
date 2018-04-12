currentFrame <- footballstats::accuracyFrame

# Define new values to be stored
newVals <- c(80.1, 75.7, 79.5, 79.4, 79.8, 79.9, 82.5)
newGroup <- 'v1.4'

newFrame <- footballstats::accuracyFrame[1:7, ]
newFrame$accuracy <- NULL
newFrame %<>% cbind(
  data.frame(
    accuracy = newVals,
    stringsAsFactors = FALSE
  )
)
newFrame$groups <- newGroup %>% as.factor

accuracyFrame <- rbind(currentFrame, newFrame)

# Plot
g <- ggplot2::ggplot(
  data = accuracyFrame,
  ggplot2::aes(
    x = league,
    y = accuracy,
    color = groups
  )) %>%
  `+`(ggplot2::geom_point(size = 5)) %>%
  `+`(ggplot2::geom_point(size = 5, shape = 21, color = 'black')) %>%
  `+`(ggplot2::ggtitle(label = 'Accuracy')) %>%
  `+`(footballstats::plot_theme())

# Save the plot
ggplot2::ggsave(
  filename = getwd() %>% paste0('/plots/accuracy.png'),
  plot = g
)

# Save the new accuracyFrame
save(accuracyFrame, file = getwd() %>% paste0('/data/accuracyFrame.rda'))
