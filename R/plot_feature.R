#' @title Plot Feature
#'
#' @description A function that takes the original.data
#'  and plots a specific feature.
#'
#' @export


plot_feature <- function(original.data) {
  toplot <- data.frame(
    home = original.data$possesiontime.h,
    away = original.data$possesiontime.a,
    result = original.data$res
  )

  ggplot2::ggplot(
    data = toplot,
    mapping = ggplot2::aes(
      x = home,
      y = away,
      colour = result
    )
  ) %>%
    `+`(ggplot2::geom_point())
}

