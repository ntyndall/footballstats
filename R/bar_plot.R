#' @Title Bar Plot
#' 
#' @export


bar_plot <- function(resultsToPlot) {
  newToplot <- resultsToPlot %>% 
    dplyr::group_by(month) %>% 
    dplyr::summarise(
      Count = sum(correct), 
      Total = dplyr::n()
    )
  
  newToplot$Wrong <- newToplot$Total %>% 
    `-`(newToplot$Count)
  
  p <- plotly::plot_ly(newToplot, x = ~month, y = ~Count, type = 'bar', name = 'Correct') %>%
    plotly::add_trace(y = ~Wrong, name = 'Incorrect') %>%
    plotly::layout(yaxis = list(title = 'Count'), barmode = 'group')
  
  return(p)
}