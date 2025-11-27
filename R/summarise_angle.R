#' Compute the circular mean of a set of angles (radians)
#'
#' The function returns the mean direction of a numeric vector of angles,
#' wrapping the result to the interval \[0, 2*pi).  It leverages the
#' `wrap_angle()` and `calculate_angular_difference()` helpers
#' that you already defined.
#'
#' @param ang Numeric vector of angles (radians).  May contain values outside
#'   \[0, 2*pi); they are normalised internally.
#' @return A single numeric value in \[0, 2*pi) representing the circular mean.
#' @examples
#' mean_angle(c(pi/2, 1.5 * pi))   # → 0
#' mean_angle(c(0, pi))           # → pi
#' @export
mean_angle <- function(ang) {
  ## 1. Normalise all inputs to [0, 2π)
  ang_mod <- wrap_angle(ang)

  ## 2. Represent each angle as a unit vector on the circle
  x <- cos(ang_mod)
  y <- sin(ang_mod)

  ## 3. Average the Cartesian components
  x_bar <- mean(x)
  y_bar <- mean(y)

  ## 4. Recover the mean direction with atan2 (range (-π, π])
  mu_raw <- atan2(y_bar, x_bar)

  ## 5. Convert the raw result to the canonical [0, 2π) interval.
  ##    We could simply do `mu_raw %% (2*pi)`, but using the existing
  ##    helper keeps the style consistent and makes future changes easier.
  mu <- wrap_angle(mu_raw)

  ## 6. Edge‑case handling:
  ##    When the resultant vector length is (near) zero (e.g., perfectly
  ##    opposite angles), `atan2(0,0)` returns 0.  In that situation the
  ##    mean is undefined, but returning 0 is conventional and matches the
  ##    example you gave (π/2 vs 3π/2 → 0).
  mu
}


#' Compute the circular median of a set of angles (radians)
#'
#' The function returns the mean direction of a numeric vector of angles,
#' wrapping the result to the interval \[0, 2*pi).  It leverages the
#' `wrap_angle()` and `calculate_angular_difference()` helpers
#' that you already defined.
#'
#' @param ang Numeric vector of angles (radians).  May contain values outside
#'   \[0, 2*pi); they are normalised internally.
#' @return A single numeric value in \[0, 2*pi) representing the circular mean.
#' @examples
#' median_angle(c(pi/2, 1.5 * pi))   # → 0
#' median_angle(c(0, pi))           # → pi
#' @export
median_angle <- function(ang) {
  ## 1. Normalise all inputs to [0, 2π)
  ang_mod <- wrap_angle(ang)

  ## 2. Represent each angle as a unit vector on the circle
  x <- cos(ang_mod)
  y <- sin(ang_mod)

  ## 3. Average the Cartesian components
  x_bar <- stats::median(x)
  y_bar <- stats::median(y)

  ## 4. Recover the mean direction with atan2 (range (-π, π])
  mu_raw <- atan2(y_bar, x_bar)

  ## 5. Convert the raw result to the canonical [0, 2π) interval.
  ##    We could simply do `mu_raw %% (2*pi)`, but using the existing
  ##    helper keeps the style consistent and makes future changes easier.
  mu <- wrap_angle(mu_raw)

  ## 6. Edge‑case handling:
  ##    When the resultant vector length is (near) zero (e.g., perfectly
  ##    opposite angles), `atan2(0,0)` returns 0.  In that situation the
  ##    mean is undefined, but returning 0 is conventional and matches the
  ##    example you gave (π/2 vs 3π/2 → 0).
  mu
}
