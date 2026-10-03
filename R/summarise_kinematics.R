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
#'   - Turning speed, rate, acceleration (2D only)
#'   - Course (2D only, using circular statistics)
#'
#'   Angular summaries are in the frame's declared `unit_angle`.
#'
#' @examples
#' kin <- calculate_kinematics(
#'   anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
#' )
#' summarise_kinematics(kin)
#'
#' # Mean and standard deviation instead of median and MAD
#' summarise_kinematics(kin, measures = "mean_sd")
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

  # Rotational measures are only present where they are defined (2D)
  linear_cols <- c("speed", "acceleration")
  angular_cols <- intersect(
    c("turning_speed", "turning_rate", "turning_acceleration"),
    names(data)
  )
  has_course <- "course" %in% names(data)

  # Circular statistics work in radians; report them in the frame's unit
  unit <- anicore::get_metadata(data, "unit_angle")
  circular <- function(stat) {
    rlang::quo(
      anicore::angle_from_rad(
        stat(anicore::angle_to_rad(.data$course, unit)),
        unit
      )
    )
  }

  if (measures == "median_mad") {
    stats <- list(
      median = ~ stats::median(.x, na.rm = TRUE),
      mad = ~ stats::mad(.x, na.rm = TRUE)
    )
    course <- if (has_course) {
      list(
        median_course = circular(anicore::circ_median),
        mad_course = circular(anicore::circ_mad)
      )
    }
  } else {
    stats <- list(
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ stats::sd(.x, na.rm = TRUE)
    )
    course <- if (has_course) {
      list(
        mean_course = circular(anicore::circ_mean),
        sd_course = circular(anicore::circ_sd)
      )
    }
  }

  data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(c(linear_cols, angular_cols)),
        stats,
        .names = "{.fn}_{.col}"
      ),
      !!!course,
      .groups = "drop"
    )
}

#' @rdname summarise_kinematics
#' @export
summarize_kinematics <- summarise_kinematics
