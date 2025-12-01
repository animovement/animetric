#' Compute the circular mean of angles
#'
#' Returns the mean direction of a vector of angles in radians,
#' wrapped to [0, 2*pi).
#'
#' @param ang Numeric vector of angles in radians.
#' @return Circular mean in [0, 2*pi).
#' @examples
#' mean_angle(c(pi/2, 1.5 * pi))
#' mean_angle(c(0, pi))
#' @export
mean_angle <- function(ang) {
  ang_mod <- anispace::wrap_angle(ang)

  x_bar <- mean(cos(ang_mod))
  y_bar <- mean(sin(ang_mod))

  mu_raw <- atan2(y_bar, x_bar)
  anispace::wrap_angle(mu_raw)
}


#' Compute the circular median of angles
#'
#' Returns the median direction of a vector of angles in radians,
#' wrapped to [0, 2*pi).
#'
#' @param ang Numeric vector of angles in radians.
#' @return Circular median in [0, 2*pi).
#' @examples
#' median_angle(c(pi/2, 1.5 * pi))
#' median_angle(c(0, pi))
#' @export
median_angle <- function(ang) {
  ang_mod <- anispace::wrap_angle(ang)

  x_bar <- stats::median(cos(ang_mod))
  y_bar <- stats::median(sin(ang_mod))

  mu_raw <- atan2(y_bar, x_bar)
  anispace::wrap_angle(mu_raw)
}
