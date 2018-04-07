footballstats::accuracyFrame

# Define new values to be stored
newVals <- c(68, 59.4, 65.6, 66.7, 67.3, 74.1, 66)
newGroup <- 3

newFrame <- footballstats::accuracyFrame[1:7, ]
newFrame$accuracy <- NULL
newFrame %<>% cbind(
  data.frame(
    accuracy = newVals,
    stringsAsFactors = FALSE
  )
)
newFrame$groups <- newGroup %>% as.factor

accuracyFrame <- rbind(footballstats::accuracyFrame, newFrame)

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
