#' Calculate direction
#' Calculate direction (angle) from x and y distance using the (two-argument) arc-tangent. Converts to `circular`.
#' @inheritParams calculate_distance
#' @keywords internal
calculate_direction <- function(dx, dy) {
  dplyr::if_else(
    dx == 0 & dy == 0,
    NA,
    circular::circular(atan2(dy, dx), modulo = "asis")
  )
}
