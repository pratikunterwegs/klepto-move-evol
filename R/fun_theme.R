
#' Custom theme with grey background
#'
#' @param grid Show panel grid major?
#'
#' @return None
#' @export
theme_custom <- function(grid = FALSE, ...) {
  this_theme <- ggplot2::theme_classic(...) +
    ggplot2::theme(
      legend.position = "none",
      axis.ticks.length = ggplot2::unit(1.5, units = "mm"),
      axis.line = ggplot2::element_line(
        colour = "black",
        size = 0.3),
      strip.background = element_rect(colour = NA,
                                      fill = "lightgrey")
      # panel.background = ggplot2::element_rect(
      #   fill = "grey99"
      # )
    )

  if (grid) {
    this_theme <- this_theme +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          colour = "grey70",
          size = 0.2
        )
      )
  }

  return(this_theme)
}
