#' @Title Bar Plot
#' 
#' @export


bar_plot <- function(resultsToPlot) {
  # First thing is that data must be filtered before it gets here!
  
  newToplot <- resultsToPlot %>% 
    dplyr::group_by(month) %>% 
    dplyr::summarise(
      Count = sum(correct), 
      Total = dplyr::n()
    )
  
  newToplot <- resultsToPlot %>% 
    dplyr::group_by(month) %>% 
    dplyr::summarise(
      Count = sum(winnings), 
      Total = dplyr::n()
    )
  
  # Get cumulative amount???
  newToplot$cSum <- newToplot$Count %>% cumsum
  
  newToplot$Wrong <- newToplot$Total %>% 
    `-`(newToplot$Count)
  
  # plotly::add_trace(y = ~Wrong, name = 'Incorrect') %>%
  p <- plotly::plot_ly(newToplot) %>%
    plotly::add_trace(x = ~month, y = ~Count, type = 'bar', name = 'Absolute',
              marker = list(color = '#C9EFF9'),
              hoverinfo = "text",
              text = ~paste("???")) %>%
    plotly::add_trace(x = ~month, y = ~cSum, type = 'scatter', mode = 'lines', name = 'Cumulative', yaxis = 'y2',
            line = list(color = '#45171D'),
            hoverinfo = "text",
            text = ~paste("??")) %>%
    plotly::layout(title = 'NBar + Line chart',
           xaxis = list(title = ""),
           yaxis = list(side = 'left', title = 'Abs.', showgrid = FALSE, zeroline = FALSE),
           yaxis2 = list(side = 'right', overlaying = "y", title = 'Cumulative Sum', showgrid = FALSE, zeroline = FALSE))
  
  return(p)
}