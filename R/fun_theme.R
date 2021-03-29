
#' Custom theme with grey background
#'
#' @param grid Show panel grid major?
#'
#' @return None
#' @export
theme_custom <- function(grid = FALSE,
                         landscape = FALSE,
                         ...) {
  this_theme <- ggplot2::theme_test(...)

  if (landscape) {
    this_theme <- this_theme +
      ggplot2::theme(
        legend.position = "none",
        legend.key.height = ggplot2::unit(1, units = "mm"),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
  } else {
    this_theme <- ggplot2::theme_classic(...) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5)
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
  }
  this_theme +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "italic", hjust = 0)
    )
}
