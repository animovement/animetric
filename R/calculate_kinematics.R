#' Calculate kinematic measures from trajectory data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [add_kinematics()], which takes the same arguments: functions
#' that return the frame with columns added now start with `add_`. This
#' returns exactly what it did, including the running total of distance as
#' `path_length`, which [add_kinematics()] calls `cumulative_distance`.
#'
#' @inheritParams add_kinematics
#'
#' @return As [add_kinematics()], with `path_length` in place of
#'   `cumulative_distance`.
#' @keywords internal
#' @export
calculate_kinematics <- function(data, vertical = NULL) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "calculate_kinematics()",
    "add_kinematics()"
  )
  add_kinematics(data, vertical = vertical) |>
    legacy_kinematics_names()
}

#' The column names `calculate_kinematics()` used
#'
#' @param data A frame with the columns [add_kinematics()] adds.
#' @return `data`, with `cumulative_distance` renamed `path_length`.
#' @keywords internal
legacy_kinematics_names <- function(data) {
  dplyr::rename(data, path_length = "cumulative_distance")
}
