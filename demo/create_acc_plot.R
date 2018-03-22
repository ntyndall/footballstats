# Plot
g <- ggplot2::ggplot(
  data = footballstats::accuracyFrame, 
  ggplot2::aes(
    x = league, 
    y = accuracy, 
    color = groups
  )) %>% 
  `+`(ggplot2::geom_point(size = 5)) %>%
  `+`(ggplot2::geom_point(size = 5, shape = 21, color = 'black')) %>%
  `+`(ggplot2::ggtitle(label = 'Accuracy')) %>%
  `+`(footballstats::plot_theme())
    
# Ssave the plot
ggplot2::ggsave(
  filename = getwd() %>% paste0('/plots/accuracy.png'),
  plot = g
)
