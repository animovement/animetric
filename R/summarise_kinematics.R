#' Calculate summary statistics
#'
#' Calculate summary statistics for tracks, including translational kinematics,
#' rotational kinematics, and tortuosity metrics.
#'
#' @param data A kinematics aniframe (output of `calculate_kinematics()`)
#' @param measures Measures of central tendency and dispersion. Options are
#'   `"median_mad"` (default) and `"mean_sd"`.
#'
#' @return A summarised data frame with one row per group containing:
#'
#'   **Totals:**
#'   - `total_path_length`: Total distance traveled
#'
#'   **Tortuosity metrics:**
#'   - `straightness`: Ratio of net displacement to path length (0-1)
#'   - `sinuosity`: Corrected sinuosity index (Benhamou 2004)
#'   - `emax`: Maximum expected displacement (dimensionless)
#'
#'   **Central tendency and dispersion** (prefixed with median_/mad_ or mean_/sd_):
#'   - Speed, acceleration
#'   - Angular speed, velocity, acceleration (2D only)
#'   - Heading (2D only, using circular statistics)
#'
#' @references
#' Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's
#' path. Journal of Theoretical Biology, 229(2), 209-220.
#'
#' @export
summarise_kinematics <- function(
  data,
  measures = c("median_mad", "mean_sd")
) {
  ensure_is_aniframe_kin(data)
  measures <- match.arg(measures)

  is_3d <- aniframe::is_cartesian_3d(data)

  # Calculate totals and tortuosity
  totals <- calculate_totals(data, is_3d)

  # Calculate central tendency and dispersion
  summaries <- calculate_summaries(data, measures, is_3d)

  # Combine
  summaries |>
    dplyr::left_join(totals, by = dplyr::join_by(!!!dplyr::group_vars(data))) |>
    dplyr::select(
      -dplyr::any_of(c(
        ".first_x",
        ".first_y",
        ".first_z",
        ".last_x",
        ".last_y",
        ".last_z"
      ))
    )
}

#' Calculate totals and tortuosity metrics
#' @keywords internal
calculate_totals <- function(data, is_3d = FALSE) {
  if (is_3d) {
    calculate_totals_3d(data)
  } else {
    calculate_totals_2d(data)
  }
}

#' @keywords internal
calculate_totals_2d <- function(data) {
  data |>
    dplyr::summarise(
      total_path_length = dplyr::last(.data$path_length, na_rm = TRUE) -
        dplyr::first(.data$path_length, na_rm = TRUE),
      total_angular_path_length = dplyr::last(
        .data$angular_path_length,
        na_rm = TRUE
      ) -
        dplyr::first(.data$angular_path_length, na_rm = TRUE),

      # For displacement calculation
      .first_x = dplyr::first(.data$x, na_rm = TRUE),
      .first_y = dplyr::first(.data$y, na_rm = TRUE),
      .last_x = dplyr::last(.data$x, na_rm = TRUE),
      .last_y = dplyr::last(.data$y, na_rm = TRUE),

      # For sinuosity: mean cosine of turning angles
      .mean_cos_turning = mean(cos(diff_angle(.data$heading)), na.rm = TRUE),

      .n_steps = sum(!is.na(.data$path_length)) - 1L,

      .groups = "drop"
    ) |>
    dplyr::mutate(
      net_displacement = sqrt(
        (.data$.last_x - .data$.first_x)^2 +
          (.data$.last_y - .data$.first_y)^2
      ),
      .mean_step_length = .data$total_path_length / .data$.n_steps,

      straightness = compute_straightness(
        .data$net_displacement,
        .data$total_path_length
      ),
      sinuosity = compute_sinuosity(
        .data$.mean_step_length,
        .data$.mean_cos_turning,
        method = "corrected"
      ),
      emax = compute_emax(.data$.mean_cos_turning)
    ) |>
    dplyr::select(
      -c(
        ".first_x",
        ".first_y",
        ".last_x",
        ".last_y",
        ".mean_cos_turning",
        ".mean_step_length",
        ".n_steps"
      )
    )
}

#' @keywords internal
calculate_totals_3d <- function(data) {
  data |>
    dplyr::mutate(
      # Compute cos_turning from velocity vectors within the grouped context
      .cos_turning = (.data$v_x *
        dplyr::lag(.data$v_x) +
        .data$v_y * dplyr::lag(.data$v_y) +
        .data$v_z * dplyr::lag(.data$v_z)) /
        (.data$speed * dplyr::lag(.data$speed))
    ) |>
    dplyr::summarise(
      total_path_length = dplyr::last(.data$path_length, na_rm = TRUE) -
        dplyr::first(.data$path_length, na_rm = TRUE),

      .first_x = dplyr::first(.data$x, na_rm = TRUE),
      .first_y = dplyr::first(.data$y, na_rm = TRUE),
      .first_z = dplyr::first(.data$z, na_rm = TRUE),
      .last_x = dplyr::last(.data$x, na_rm = TRUE),
      .last_y = dplyr::last(.data$y, na_rm = TRUE),
      .last_z = dplyr::last(.data$z, na_rm = TRUE),

      .mean_cos_turning = mean(.data$.cos_turning, na.rm = TRUE),
      .n_steps = sum(!is.na(.data$path_length)) - 1L,

      .groups = "drop"
    ) |>
    dplyr::mutate(
      net_displacement = sqrt(
        (.data$.last_x - .data$.first_x)^2 +
          (.data$.last_y - .data$.first_y)^2 +
          (.data$.last_z - .data$.first_z)^2
      ),
      .mean_step_length = .data$total_path_length / .data$.n_steps,

      straightness = compute_straightness(
        .data$net_displacement,
        .data$total_path_length
      ),
      sinuosity = compute_sinuosity(
        .data$.mean_step_length,
        .data$.mean_cos_turning,
        method = "corrected"
      ),
      emax = compute_emax(.data$.mean_cos_turning)
    ) |>
    dplyr::select(
      -c(
        ".first_x",
        ".first_y",
        ".first_z",
        ".last_x",
        ".last_y",
        ".last_z",
        ".mean_cos_turning",
        ".mean_step_length",
        ".n_steps"
      )
    )
}

#' Calculate summary statistics for kinematic measures
#' @keywords internal
calculate_summaries <- function(data, measures, is_3d = FALSE) {
  if (is_3d) {
    calculate_summaries_3d(data, measures)
  } else {
    calculate_summaries_2d(data, measures)
  }
}

#' @keywords internal
calculate_summaries_2d <- function(data, measures) {
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
calculate_summaries_3d <- function(data, measures) {
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
