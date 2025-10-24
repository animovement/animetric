#' Calculate direction
#' Calculate direction (angle) in 2D or 3D.
#' @inheritParams calculate_distance
#' @keywords internal
calculate_direction <- function(dx, dy, dz = NULL) {
  if (is.null(dz)) {
    calculate_direction_2d(dx, dy)
  } else {
    calculate_direction_3d(dx, dy, dz)
  }
}

#' Calculate direction
#' Calculate direction (angle) from x and y distance using the (two-argument) arc-tangent.
#' @inheritParams calculate_distance
#' @keywords internal
calculate_direction_2d <- function(dx, dy) {
  dplyr::if_else(
    dx == 0 & dy == 0,
    NA,
    atan2(dy, dx)
  )
}

#' Calculate direction
#' Calculate direction (angle) from x, y and z distance using ...
#' @inheritParams calculate_distance
#' @keywords internal
calculate_direction_3d <- function(dx, dy, dz) {
  cli::cli_abort("We currently don't support 3D direction.")
}
