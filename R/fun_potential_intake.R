#' Get potential intake rate from item count.
#'
#' @param p_detection Prob. detecting a single item.
#' @param n_items Items on the cell.
#'
#' @return A potential intake rate.
#' @export
#'
get_potential_intake = function(p_detection, n_items) {
  1 - (1 - p_detection) ^ n_items
}