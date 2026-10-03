#' Calculate kinematic summary statistics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [summarise_aniframe()], which summarises any per-row measure of a
#' frame, not only kinematics. This keeps the old output: the median and MAD
#' (or mean and SD) of `speed`, `acceleration`, the turning measures,
#' `course_elevation` and, with circular statistics, `course`.
#'
#' @param data An anipoint with kinematic columns.
#' @param measures `"median_mad"` (default) or `"mean_sd"`.
#' @param .check Ignored.
#'
#' @return A data frame with one row per group.
#' @keywords internal
#' @export
#' @aliases summarize_kinematics
summarise_kinematics <- function(
  data,
  measures = c("median_mad", "mean_sd"),
  .check = TRUE
) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "summarise_kinematics()",
    "summarise_aniframe()"
  )
  summarise_kinematics_legacy(data, match.arg(measures))
}

#' @rdname summarise_kinematics
#' @export
summarize_kinematics <- summarise_kinematics

#' What `summarise_kinematics()` returned
#'
#' @param data An anipoint.
#' @param measures `"median_mad"` or `"mean_sd"`.
#' @return A data frame with one row per group.
#' @keywords internal
summarise_kinematics_legacy <- function(data, measures) {
  anicore::ensure_is_anipoint(data)
  cols <- intersect(
    c(
      "speed",
      "acceleration",
      "turning_speed",
      "turning_rate",
      "turning_acceleration",
      "course_elevation",
      "course"
    ),
    names(data)
  )
  summarise_aniframe.anipoint(data, cols = cols, measures = measures)
}
