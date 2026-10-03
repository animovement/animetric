#' @keywords internal
cumsum_na <- function(x) {
  cumsum(dplyr::coalesce(x, 0))
}

#' Cumulative absolute turning of an unwrapped angle
#'
#' Starts at 0 and accumulates the absolute change between consecutive
#' non-`NA` angles, so turning across a gap is counted once, where the angle
#' is next defined. Rows with an `NA` angle carry the running total.
#'
#' @param x Numeric vector of unwrapped angles.
#' @return Numeric vector of the same length as `x`.
#' @keywords internal
cumsum_turning <- function(x) {
  defined <- !is.na(x)
  steps <- numeric(length(x))
  steps[defined] <- c(0, abs(diff(x[defined])))[seq_len(sum(defined))]
  cumsum(steps)
}
