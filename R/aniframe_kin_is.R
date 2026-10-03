#' Test whether a frame holds kinematics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' The `aniframe_kin` class is retired: it only labelled a frame as having
#' been through [calculate_kinematics()], said nothing about which columns
#' it held, and outlived them (`select(-speed)` kept it). Check for the
#' columns you need instead, e.g. `"speed" %in% names(x)`.
#'
#' @param x An object.
#'
#' @return `TRUE` for an anipoint with a `speed` column.
#' @keywords internal
#' @export
is_aniframe_kin <- function(x) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "is_aniframe_kin()",
    details = "Check for the columns you need instead, e.g. `\"speed\" %in% names(x)`."
  )
  anicore::is_anipoint(x) && "speed" %in% names(x)
}
