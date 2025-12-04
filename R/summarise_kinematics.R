#' Calculate kinematic summary statistics
#'
#' Calculate central tendency and dispersion for translational and rotational
#' kinematics.
#'
#' @inheritParams summarise_aniframe
#' @param .check Whether to validate input. Set to `FALSE` when called from
#'   `summarise_aniframe()` to avoid redundant checks.
#'
#' @return A summarised data frame with one row per group containing central
#'   tendency and dispersion measures (prefixed with median_/mad_ or mean_/sd_)
#'
#'   - Speed, acceleration
#'   - Angular speed, velocity, acceleration (2D only)
#'   - Heading (2D only, using circular statistics)
#'
#' @export
#' @aliases summarize_kinematics
summarise_kinematics <- function(
  data,
  measures = c("median_mad", "mean_sd"),
  .check = TRUE
) {
  if (.check) {
    ensure_is_aniframe_kin(data)
  }
  measures <- match.arg(measures)

  is_3d <- aniframe::is_cartesian_3d(data)

  if (is_3d) {
    summarise_kinematics_3d(data, measures)
  } else {
    summarise_kinematics_2d(data, measures)
  }
}

#' @rdname summarise_kinematics
#' @export
summarize_kinematics <- summarise_kinematics

#' @keywords internal
summarise_kinematics_2d <- function(data, measures) {
  linear_cols <- c("speed", "acceleration")
  angular_cols <- c("angular_speed", "angular_velocity", "angular_acceleration")

  if (measures == "median_mad") {
    data |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(c(linear_cols, angular_cols)),
          list(
            median = ~ stats::median(.x, na.rm = TRUE),
            mad = ~ stats::mad(.x, na.rm = TRUE)
          ),
          .names = "{.fn}_{.col}"
        ),
        median_heading = circ_median(.data$heading),
        mad_heading = circ_mad(.data$heading),
        .groups = "drop"
      )
  } else {
    data |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(c(linear_cols, angular_cols)),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ stats::sd(.x, na.rm = TRUE)
          ),
          .names = "{.fn}_{.col}"
        ),
        mean_heading = as.numeric(mean(
          circular::circular(.data$heading),
          na.rm = TRUE
        )),
        sd_heading = circ_sd(.data$heading),
        .groups = "drop"
      )
  }
}


#' @keywords internal
summarise_kinematics_3d <- function(data, measures) {
  linear_cols <- c("speed", "acceleration")

  if (measures == "median_mad") {
    data |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(linear_cols),
          list(
            median = ~ stats::median(.x, na.rm = TRUE),
            mad = ~ stats::mad(.x, na.rm = TRUE)
          ),
          .names = "{.fn}_{.col}"
        ),
        .groups = "drop"
      )
  } else {
    data |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(linear_cols),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ stats::sd(.x, na.rm = TRUE)
          ),
          .names = "{.fn}_{.col}"
        ),
        .groups = "drop"
      )
  }
}
