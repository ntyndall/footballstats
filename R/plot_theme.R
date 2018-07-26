#' @title Plot Theme
#'
#' @description Holds the theme for the plots to be used in the
#'  README
#'
#' @param fontFamily A character string that defines the font
#'  family to be used for the axis / title / legend etc.
#'
#' @export


plot_theme <- function(fontFamily = 'Purisa', titleFont = 26) { # nocov start
  return(
    ggplot2::theme(
      plot.title =  ggplot2::element_text(
        family = 'Purisa',
        face = 'plain',
        colour = '#353535',
        size = titleFont,
        hjust = 0.5
      ),
      legend.title = ggplot2::element_text(
        family = 'Purisa',
        face =  'plain',
        colour = '#353535',
        size = 20
      ),
      legend.text = ggplot2::element_text(
        family = 'Purisa',
        face = 'plain',
        colour = "#353535",
        size = 14
      ),
      legend.key = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        family = 'Purisa',
        face =  'plain',
        colour = '#353535',
        size = 12,
        angle = 20,
        hjust = 1
      ),
      axis.text.y = ggplot2::element_text(
        family = 'Purisa',
        face = 'plain',
        colour = '#353535',
        size = 12
      ),
      panel.border = ggplot2::element_rect(
        linetype = "dashed",
        colour = '#353535',
        fill = NA
      ),
      text = ggplot2::element_text(
        family = 'Purisa',
        face = 'plain',
        colour = '#353535',
        size = 20
      ),
      panel.background = ggplot2::element_rect(
        fill = 'white'
      ),
      plot.background = ggplot2::element_rect(
        fill = "white"
      ),
      legend.background = ggplot2::element_rect(
        fill = "white",
        size = 0.5
      ),
      panel.grid.major = ggplot2::element_line(
        colour = "#353535",
        size = 0.1,
        linetype = 'dotted'
      ),
      panel.grid.minor = ggplot2::element_line(
        colour = "#353535",
        size = 0.1,
        linetype = 'dotted'
      )
    )
  )
} # nocov end
