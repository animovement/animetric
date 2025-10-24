#' Calculate distance (Pythagoras)
#' Calculate distance from an x and y distance, using Pythagoras theorem.
#' @param dx dx
#' @param dy dy
#' @param dz dz
#' @keywords internal
calculate_distance <- function(dx, dy, dz = NULL) {
  aniframe::cartesian_to_rho(dx, dy, dz)
}
