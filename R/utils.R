#' @keywords internal
cumsum_na <- function(x) {
  cumsum(dplyr::coalesce(x, 0))
}

#' Cumulative absolute turning of an angle series
#'
#' Starts at 0 and accumulates the absolute turn between consecutive
#' non-`NA` angles, taken the shorter way round the circle, so turning across
#' a gap is counted once, where the angle is next defined. Rows with an `NA`
#' angle carry the running total.
#'
#' @param x Numeric vector of angles, in radians. They need not be unwrapped.
#' @return Numeric vector of the same length as `x`.
#' @keywords internal
cumsum_turning <- function(x) {
  defined <- !is.na(x)
  steps <- numeric(length(x))
  turns <- abs(anicore::circ_successive_difference(x[defined]))
  steps[defined] <- dplyr::coalesce(turns, 0)
  cumsum(steps)
}
