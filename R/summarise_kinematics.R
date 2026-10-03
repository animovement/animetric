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
    c("angular_speed", "angular_velocity", "angular_acceleration"),
    names(data)
  )
  has_heading <- "heading" %in% names(data)

  if (measures == "median_mad") {
    stats <- list(
      median = ~ stats::median(.x, na.rm = TRUE),
      mad = ~ stats::mad(.x, na.rm = TRUE)
    )
    heading <- if (has_heading) {
      list(
        median_heading = rlang::quo(anicore::circ_median(.data$heading)),
        mad_heading = rlang::quo(anicore::circ_mad(.data$heading))
      )
    }
  } else {
    stats <- list(
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ stats::sd(.x, na.rm = TRUE)
    )
    heading <- if (has_heading) {
      list(
        mean_heading = rlang::quo(anicore::circ_mean(.data$heading)),
        sd_heading = rlang::quo(anicore::circ_sd(.data$heading))
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
      !!!heading,
      .groups = "drop"
    )
}

#' @rdname summarise_kinematics
#' @export
summarize_kinematics <- summarise_kinematics
