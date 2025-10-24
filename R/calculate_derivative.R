#' Numerical derivative of arbitrary order
#'
#' Computes the n‑th derivative of a 1‑D signal `x` sampled at timestamps
#' `time`.  The algorithm mirrors `xarray.DataArray.differentiate`:
#'   * forward difference at the first point,
#'   * central differences for interior points,
#'   * backward difference at the last point.
#' The result always has the **same length** as the input vectors.
#'
#' @param x     Numeric vector of the same length as `time` (e.g., position/distance).
#' @param time  Numeric, strictly increasing vector of timestamps.
#' @param order Positive integer indicating the derivative order (1 = first,
#'              2 = second, …).  Defaults to 1.
#'
#' @return A numeric vector of length `length(time)` containing the
#'         `order`‑th derivative.
#'
#' @examples
#' # Linear motion: x = t  →  1st derivative = 1, 2nd = 0, 3rd = 0 …
#' t <- 0:5
#' x <- 0:5
#' calculate_derivative(x, t, order = 1L)  # => c(1,1,1,1,1,1)
#' calculate_derivative(x, t, order = 2L)  # => c(0,0,0,0,0,0)
#' calculate_derivative(x, t, order = 3L)  # => c(0,0,0,0,0,0)
#'
#' @export
calculate_derivative <- function(x, time, order = 1L) {
  ## ----------------------------------------------------------------------
  ## 1️⃣  Input validation
  ## ----------------------------------------------------------------------
  if (!is.numeric(time) || !is.numeric(x)) {
    cli::cli_abort("Both 'time' and 'x' must be numeric vectors.")
  }
  if (length(time) != length(x)) {
    cli::cli_abort("'time' and 'x' must have the same length.")
  }
  if (any(diff(time) <= 0)) {
    cli::cli_abort("'time' must be strictly increasing.")
  }
  if (!is.integer(order) || length(order) != 1L || order < 1L) {
    cli::cli_abort("'order' must be a single positive integer.")
  }
  order <- as.integer(order) # coerce safely

  result <- x
  for (k in seq_len(order)) {
    result <- first_derivative(time, result)
  }

  result
}

#' @keywords internal
first_derivative <- function(t, y) {
  n <- length(t)
  dt <- diff(t) # length n‑1
  der <- numeric(n)

  # forward difference (first point)
  der[1] <- (y[2] - y[1]) / dt[1]

  # central differences for interior points (if any)
  if (n > 2) {
    der[2:(n - 1)] <-
      (y[3:n] - y[1:(n - 2)]) / (t[3:n] - t[1:(n - 2)])
  }

  # backward difference (last point)
  der[n] <- (y[n] - y[n - 1]) / dt[n - 1]

  der
}
