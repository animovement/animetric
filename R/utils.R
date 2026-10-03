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

#' The angular unit a frame declares
#'
#' @param data An aniframe.
#' @return `"deg"` or `"rad"`. Anything other than degrees is read as radians,
#'   which is what the angular computations use internally.
#' @keywords internal
angle_unit <- function(data) {
  unit <- as.character(anicore::get_metadata(data, "unit_angle"))
  if (identical(unit, "deg")) "deg" else "rad"
}

#' Convert angles between radians and a frame's angular unit
#'
#' The angular computations work in radians. `rad_to_unit()` expresses their
#' results in the frame's unit, and `unit_to_rad()` reads the frame's angles
#' back into radians.
#'
#' @param x Numeric vector of angles, or of angular rates.
#' @param unit `"rad"` or `"deg"`, as from [angle_unit()].
#' @return Numeric vector.
#' @keywords internal
rad_to_unit <- function(x, unit) {
  if (unit == "deg") anicore::rad_to_deg(x) else x
}

#' @rdname rad_to_unit
#' @keywords internal
unit_to_rad <- function(x, unit) {
  if (unit == "deg") anicore::deg_to_rad(x) else x
}
