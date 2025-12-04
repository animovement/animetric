#' Calculate tortuosity summary statistics
#'
#' Calculate path length, displacement, and tortuosity metrics.
#'
#' @inheritParams summarise_aniframe
#'
#' @return A summarised data frame with one row per group containing:
#'
#'   - `total_path_length`: Total distance traveled
#'   - `total_angular_path_length`: Total angular distance (2D only)
#'
#'   **Tortuosity metrics:**
#'   - `net_displacement`: Straight-line distance from start to end
#'   - `straightness`: Ratio of net displacement to path length (0-1)
#'   - `sinuosity`: Corrected sinuosity index (Benhamou 2004)
#'   - `emax`: Maximum expected displacement (dimensionless)
#'
#' @references
#' Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's
#' path. Journal of Theoretical Biology, 229(2), 209-220.
#'
#' @export
#' @aliases summarize_tortuosity
summarise_tortuosity <- function(data) {
  is_3d <- aniframe::is_cartesian_3d(data)

  if (!is_aniframe_kin(data)) {
    data <- data |>
      calculate_kinematics() |>
      calculate_tortuosity()
  }

  if (is_3d) {
    summarise_tortuosity_3d(data)
  } else {
    summarise_tortuosity_2d(data)
  }
}

#' @rdname summarise_tortuosity
#' @export
summarize_tortuosity <- summarise_tortuosity

#' @keywords internal
summarise_tortuosity_2d <- function(data) {
  data |>
    dplyr::summarise(
      total_path_length = dplyr::last(.data$path_length, na_rm = TRUE) -
        dplyr::first(.data$path_length, na_rm = TRUE),
      total_angular_path_length = dplyr::last(
        .data$angular_path_length,
        na_rm = TRUE
      ) -
        dplyr::first(.data$angular_path_length, na_rm = TRUE),

      .first_x = dplyr::first(.data$x, na_rm = TRUE),
      .first_y = dplyr::first(.data$y, na_rm = TRUE),
      .last_x = dplyr::last(.data$x, na_rm = TRUE),
      .last_y = dplyr::last(.data$y, na_rm = TRUE),

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
summarise_tortuosity_3d <- function(data) {
  data |>
    dplyr::mutate(
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
