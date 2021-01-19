
#' Custom theme with grey background
#'
#' @param grid Show panel grid major?
#'
#' @return None
#' @export
theme_custom <- function(grid = FALSE) {
  this_theme = ggplot2::theme_test(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_rect(
        fill = "grey99"
      )
    )
  
  if (grid) {
    this_theme = this_theme +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          colour = "grey70",
          size = 0.2
        )
      )
  }
  
  return(this_theme)
}
