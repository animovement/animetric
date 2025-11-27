#' Compute numerical derivatives on possibly uneven grids
#'
#' This helper implements a finite‑difference scheme based on the
#' **Fornberg formula** for interior points when the spacing between
#' coordinates is non‑uniform. End points use first‑order forward/backward
#' differences.
#'
#' @param values Numeric vector of function values.
#' @param coords Numeric vector of the corresponding coordinate values.
#'   Must be the same length as `values`.
#' @return A numeric vector of the same length as `values` containing the
#'   estimated first derivative at each point.
#' @keywords internal
compute_gradient <- function(values, coords = NULL) {
  n <- length(values)

  if (is.null(coords)) {
    coords <- seq_along(1:n)
  }

  if (n < 2) {
    stop("Need at least 2 points")
  }

  derivs <- numeric(n)
  coord_diffs <- diff(coords)

  ## First point – forward difference (first order)
  derivs[1] <- (values[2] - values[1]) / coord_diffs[1]

  ## Interior points – central difference using Fornberg's formula
  if (n > 2) {
    for (i in 2:(n - 1)) {
      h_back <- coord_diffs[i - 1] # coords[i]   - coords[i-1]
      h_forward <- coord_diffs[i] # coords[i+1] - coords[i]

      # Use simple centered difference for uniform spacing
      if (h_back == h_forward) {
        derivs[i] <- (values[i + 1] - values[i - 1]) / (2 * h_back)
      } else {
        # Fornberg formula for uneven spacing
        numerator <- (h_back^2 *
          values[i + 1] +
          (h_forward^2 - h_back^2) * values[i] -
          h_forward^2 * values[i - 1])
        denominator <- h_back * h_forward * (h_back + h_forward)

        derivs[i] <- numerator / denominator
      }
    }
  }

  ## Last point – backward difference (first order)
  derivs[n] <- (values[n] - values[n - 1]) / coord_diffs[n - 1]

  derivs
}


#' Differentiate a numeric series (optionally repeatedly)
#'
#' Wrapper around `compute_gradient()` that optionally applies the gradient
#' operator multiple times (`order`).  If no explicit time vector is supplied,
#' a simple index sequence is used.
#'
#' @param x Numeric vector of observations.
#' @param time Optional numeric vector of timestamps.  Must be the same length
#'   as `x`.  If `NULL`, `seq_along(x)` is used.
#' @param order Integer ≥ 1 indicating how many times the differentiation
#'   should be applied.  Defaults to a single derivative.
#' @return Numeric vector of the same length as `x` containing the
#'   differentiated values.
#' @details
#'   The function computes the first‑order derivative using the
#'   Fornberg‑based scheme implemented in `compute_gradient()`.  When
#'   `order > 1`, the gradient is applied iteratively to the result of the
#'   previous iteration.
#' @examples
#'   # Simple equally spaced case
#'   y <- sin(seq(0, 2 * pi, length.out = 10))
#'   differentiate(y)
#'
#'   # Uneven time stamps
#'   t <- c(0, 0.9, 2.1, 3.8, 5.0, 5.2, 5.6, 6.7, 7.2, 8.9)
#'   differentiate(y, time = t, order = 2)
#' @export
differentiate <- function(x, time = NULL, order = 1) {
  len <- length(x)

  if (is.null(time)) {
    time <- seq_along(x)
  }

  if (length(time) != len) {
    stop("x and time must have the same length")
  }

  # Apply the gradient `order` times
  result <- x
  for (i in seq_len(order)) {
    result <- compute_gradient(result, time)
  }

  result
}
