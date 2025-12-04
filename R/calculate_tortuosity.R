# ============================================================================
# CALCULATE FUNCTIONS (aniframe in, aniframe with new columns out)
# ============================================================================

#' Calculate tortuosity metrics over sliding windows
#'
#' Computes multiple tortuosity metrics (straightness, sinuosity, E_max) over
#' sliding windows, returning a value at each timepoint.
#'
#' If required kinematic columns are missing, the function will compute them
#' automatically by calling the appropriate helper functions.
#'
#' @param data An aniframe with position coordinates and time. Velocity and
#'   heading columns will be computed if not already present.
#' @param window_width Size of the sliding window (number of observations).
#'   Should be an odd number >= 3 for symmetric centering.
#'
#' @return The input aniframe with additional columns:
#'   \describe{
#'     \item{straightness}{Straightness index (D/L), ranges 0-1}
#'     \item{sinuosity}{Corrected sinuosity index (Benhamou 2004)}
#'     \item{emax}{Maximum expected displacement (dimensionless)}
#'   }
#'
#' @details
#' Straightness is appropriate for directed/goal-oriented movement, while
#' sinuosity and E_max are appropriate for random search paths.
#'
#' For 2D data, heading is derived from the velocity vector, which provides
#' smoother estimates than raw position differences.
#'
#' For 3D data, turning angles are computed as the angle between consecutive
#' velocity vectors using the dot product.
#'
#' The window is centered on each timepoint. At path edges, metrics are
#' computed from available data within the truncated window.
#'
#' @references
#' Batschelet, E. (1981). Circular statistics in biology. Academic Press.
#'
#' Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal’s
#' path: straightness, sinuosity, or fractal dimension?.
#' Journal of Theoretical Biology, 229(2), 209-220.
#'
#' Cheung, A., Zhang, S., Stricker, C., & Srinivasan, M. V. (2007). Animal
#' navigation: the difficulty of moving in a straight line. Biological
#' Cybernetics, 97(1), 47-61.
#'
#' @seealso
#' * [calculate_kinematics()] for computing velocity and heading
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Kinematics computed automatically if missing
#' data |>
#'   calculate_tortuosity(window_width = 11)
#'
#' # Or with kinematics already computed
#' data |>
#'   calculate_kinematics() |>
#'   calculate_tortuosity(window_width = 11)
#' }
calculate_tortuosity <- function(data, window_width = 11L) {
  if (aniframe::is_cartesian_2d(data)) {
    calculate_tortuosity_2d(data, window_width = window_width) |> 
      aniframe::as_aniframe()
  } else if (aniframe::is_cartesian_3d(data)) {
    calculate_tortuosity_3d(data, window_width = window_width) |> 
      aniframe::as_aniframe()
  } else {
    cli::cli_abort("Data must be in Cartesian coordinates (2D or 3D).")
  }
}

#' @keywords internal
calculate_tortuosity_2d <- function(data, window_width = 11L) {
  # Check that data.table is installed
  dt_check()

  # Validate that it is an aniframe
  ensure_is_aniframe(data)

  # Validate window_width
  window_width <- as.integer(window_width)
  if (window_width < 3L) {
    cli::cli_abort("{.arg window_width} must be at least 3.")
  }

  # Ensure we have the required columns
  # heading requires v_x, v_y which require translation kinematics
  if (!"heading" %in% names(data)) {
    if (!all(c("v_x", "v_y") %in% names(data))) {
      data <- calculate_translation_2d(data)
    }
    data <- calculate_rotation_2d(data)
  }

  half_w <- window_width %/% 2L
  other_half <- window_width - half_w - 1L

  data2 <- data |>
    dplyr::mutate(
      # Step length between consecutive points
      .step_length = sqrt(
        (.data$x - dplyr::lag(.data$x))^2 +
          (.data$y - dplyr::lag(.data$y))^2
      ),

      # Turning angle from heading (already computed from velocity vector)
      .turning = diff_angle(.data$heading),
      .cos_turning = cos(.data$.turning),

      # Rolling sums using data.table (fast algorithm, centered window)
      .roll_sum_step = data.table::frollsum(
        .data$.step_length,
        n = window_width - 1L,
        algo = "fast",
        align = "center",
        na.rm = TRUE
      ),
      .roll_count_step = data.table::frollsum(
        as.numeric(!is.na(.data$.step_length)),
        n = window_width - 1L,
        algo = "fast",
        align = "center"
      ),

      .roll_sum_cos = data.table::frollsum(
        .data$.cos_turning,
        n = window_width - 1L,
        algo = "fast",
        align = "center",
        na.rm = TRUE
      ),
      .roll_count_cos = data.table::frollsum(
        as.numeric(!is.na(.data$.cos_turning)),
        n = window_width - 1L,
        algo = "fast",
        align = "center"
      ),

      # Window displacement using lag/lead for start/end positions
      .x_start = dplyr::lag(.data$x, n = half_w, default = NA),
      .y_start = dplyr::lag(.data$y, n = half_w, default = NA),
      .x_end = dplyr::lead(.data$x, n = other_half, default = NA),
      .y_end = dplyr::lead(.data$y, n = other_half, default = NA),
      .x_diff = .data$.x_end - .data$.x_start,
      .y_diff = .data$.y_end - .data$.y_start,
      .window_displacement = sqrt(.data$.x_diff^2 + .data$.y_diff^2),

      # Compute means
      .window_path_length = .data$.roll_sum_step,
      .mean_step = .data$.roll_sum_step / .data$.roll_count_step,
      .mean_cos = .data$.roll_sum_cos / .data$.roll_count_cos
    ) |>
    dplyr::mutate(
      # Final metrics
      straightness = compute_straightness(
        .data$.window_displacement,
        .data$.window_path_length
      ),
      sinuosity = compute_sinuosity(
        .data$.mean_step,
        .data$.mean_cos,
        method = "corrected"
      ),
      emax = compute_emax(.data$.mean_cos)
    ) |>
    dplyr::select(-dplyr::starts_with("."))
}

#' @keywords internal
calculate_tortuosity_3d <- function(data, window_width = 11L) {
  # Check that data.table is installed
  dt_check()

  # Validate that it is an aniframe
  aniframe::ensure_is_aniframe(data)

  window_width <- as.integer(window_width)
  if (window_width < 3L) {
    cli::cli_abort("{.arg window_width} must be at least 3.")
  }

  # Ensure we have velocity columns
  # For 3D, we compute turning angle directly from velocity vectors
  if (!all(c("v_x", "v_y", "v_z", "speed") %in% names(data))) {
    data <- calculate_translation_3d(data)
  }

  half_w <- window_width %/% 2L
  other_half <- window_width - half_w - 1L

  data |>
    dplyr::mutate(
      # Step length (3D)
      .step_length = sqrt(
        (.data$x - dplyr::lag(.data$x))^2 +
          (.data$y - dplyr::lag(.data$y))^2 +
          (.data$z - dplyr::lag(.data$z))^2
      ),

      # 3D turning angle via dot product of consecutive velocity vectors
      # cos(theta) = (v1 · v2) / (|v1| |v2|)
      .dot_product = (.data$v_x *
        dplyr::lag(.data$v_x) +
        .data$v_y * dplyr::lag(.data$v_y) +
        .data$v_z * dplyr::lag(.data$v_z)),
      .speed_product = .data$speed * dplyr::lag(.data$speed),
      .cos_turning = .data$.dot_product / .data$.speed_product,

      # Rolling sums using data.table (fast algorithm, centered window)
      .roll_sum_step = data.table::frollsum(
        .data$.step_length,
        n = window_width - 1L,
        algo = "fast",
        align = "center",
        na.rm = TRUE
      ),
      .roll_count_step = data.table::frollsum(
        as.numeric(!is.na(.data$.step_length)),
        n = window_width - 1L,
        algo = "fast",
        align = "center"
      ),
      .roll_sum_cos = data.table::frollsum(
        .data$.cos_turning,
        n = window_width - 1L,
        algo = "fast",
        align = "center",
        na.rm = TRUE
      ),
      .roll_count_cos = data.table::frollsum(
        as.numeric(!is.na(.data$.cos_turning)),
        n = window_width - 1L,
        algo = "fast",
        align = "center"
      ),

      # Window displacement (3D)
      .x_start = dplyr::lag(
        .data$x,
        n = half_w,
        default = dplyr::first(.data$x)
      ),
      .y_start = dplyr::lag(
        .data$y,
        n = half_w,
        default = dplyr::first(.data$y)
      ),
      .z_start = dplyr::lag(
        .data$z,
        n = half_w,
        default = dplyr::first(.data$z)
      ),
      .x_end = dplyr::lead(
        .data$x,
        n = other_half,
        default = dplyr::last(.data$x)
      ),
      .y_end = dplyr::lead(
        .data$y,
        n = other_half,
        default = dplyr::last(.data$y)
      ),
      .z_end = dplyr::lead(
        .data$z,
        n = other_half,
        default = dplyr::last(.data$z)
      ),
      .window_displacement = sqrt(
        (.data$.x_end - .data$.x_start)^2 +
          (.data$.y_end - .data$.y_start)^2 +
          (.data$.z_end - .data$.z_start)^2
      ),

      # Means
      .window_path_length = .data$.roll_sum_step,
      .mean_step = .data$.roll_sum_step / .data$.roll_count_step,
      .mean_cos = .data$.roll_sum_cos / .data$.roll_count_cos
    ) |>
    dplyr::mutate(
      straightness = compute_straightness(
        .data$.window_displacement,
        .data$.window_path_length
      ),
      sinuosity = compute_sinuosity(
        .data$.mean_step,
        .data$.mean_cos,
        method = "corrected"
      ),
      emax = compute_emax(.data$.mean_cos)
    ) |>
    dplyr::select(-dplyr::starts_with("."))
}
