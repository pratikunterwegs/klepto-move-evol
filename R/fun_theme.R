
#' Custom theme with grey background
#'
#' @param grid Show panel grid major?
#'
#' @return None
#' @export
theme_custom <- function(grid = FALSE,
                         landscape = FALSE,
                         ...) {
  this_theme <- ggplot2::theme_test(...) +
    ggplot2::theme(
      legend.position = "none",
      axis.title = ggplot2::element_text(face = "bold"),
      # axis.ticks.length = ggplot2::unit(1.5, units = "mm"),
      axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
      # axis.line = ggplot2::element_line(
      #   colour = "black",
      #   size = 0.3),
      # strip.background = ggplot2::element_rect(
      #   colour = "grey20",
      #   fill = "lightgrey",
      #   size = 0.3
      # ),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.background = ggplot2::element_rect(
        fill = "grey99"
      )
    )

  if (grid) {
    this_theme <- this_theme +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          colour = "grey70",
          size = 0.1
        )
      )
  }

  if (landscape) {
    this_theme <- this_theme +
      ggplot2::theme(
        legend.position = "top",
        legend.key.height = ggplot2::unit(1, units = "mm"),
        axis.text = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
  }

  return(this_theme)
}
