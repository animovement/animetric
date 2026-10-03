# ============================================================================
# CALCULATE FUNCTIONS (anipoint in, anipoint with new columns out)
# ============================================================================

#' Calculate tortuosity metrics over sliding windows
#'
#' Computes multiple tortuosity metrics (straightness, sinuosity, E_max) over
#' sliding windows, returning a value at each timepoint.
#'
#' If required kinematic columns are missing, the function will compute them
#' automatically by calling the appropriate helper functions.
#'
#' @param data A Cartesian anipoint. Kinematic columns will be computed if not
#'   already present.
#' @param window_width Size of the sliding window (number of observations).
#'   Should be an odd number >= 3 for symmetric centering.
#'
#' @return The input anipoint with additional columns:
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
#' Works on 1D, 2D and 3D Cartesian data, reading the axes from the frame's
#' declared variables. Turning angles are the angles between consecutive
#' velocity vectors ([anicore::angle_between()]), which gives smoother
#' estimates than raw position differences.
#'
#' The window is centered on each timepoint. Near the ends of a trajectory,
#' where the window would run past the first or last observation, the metrics
#' are `NA`.
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
#' data <- anicore::example_anipoint(n_obs = 30, n_individuals = 1, n_keypoints = 1)
#'
#' # Kinematics computed automatically if missing
#' data |>
#'   calculate_tortuosity(window_width = 11)
#'
#' # Or with kinematics already computed
#' data |>
#'   calculate_kinematics() |>
#'   calculate_tortuosity(window_width = 11)
calculate_tortuosity <- function(data, window_width = 11L) {
  ensure_trajectory_grouping(data)

  # Validate that it is a Cartesian anipoint
  anicore::ensure_is_anipoint(data)
  if (!anicore::is_cartesian(data)) {
    cli::cli_abort("Data must be in Cartesian coordinates.")
  }

  # Validate window_width
  window_width <- as.integer(window_width)
  if (window_width < 3L) {
    cli::cli_abort("{.arg window_width} must be at least 3.")
  }

  # Store original class for restoration
  original_class <- class(data)

  axes <- cartesian_axes(data)
  position_cols <- unname(axes)
  v_cols <- paste0("v_", names(axes))

  # Turning angles come from the velocity vectors
  if (!all(c(v_cols, "speed") %in% names(data))) {
    data <- add_kinematics(data)
  }

  half_w <- window_width %/% 2L
  other_half <- window_width - half_w - 1L

  result <- data |>
    dplyr::mutate(
      # Step length between consecutive points
      .step_length = step_length(dplyr::pick(dplyr::all_of(position_cols))),

      # Turning angle between consecutive velocity vectors
      .cos_turning = cos_turning(dplyr::pick(dplyr::all_of(v_cols))),

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

      # Displacement between the window's start and end positions
      .window_displacement = vector_norm(lapply(
        dplyr::pick(dplyr::all_of(position_cols)),
        \(x) dplyr::lead(x, n = other_half) - dplyr::lag(x, n = half_w)
      )),

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

  # Restore original class
  class(result) <- original_class
  result
}
