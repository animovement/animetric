#' Calculate tortuosity metrics over sliding windows
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [add_tortuosity()], which takes the same arguments. This
#' returns exactly what it did: columns `straightness`, `sinuosity` and
#' `emax`, plus every kinematic column of [calculate_kinematics()] when the
#' frame did not already have velocities and speed. [add_tortuosity()] adds
#' only its three measures, with the window width in their names
#' (`straightness_11`, `sinuosity_11`, `e_max_11`).
#'
#' @inheritParams add_tortuosity
#'
#' @return The input anipoint with `straightness`, `sinuosity` and `emax`
#'   added, and the kinematic columns if they were missing.
#' @keywords internal
#' @export
calculate_tortuosity <- function(data, window_width = 11L) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "calculate_tortuosity()",
    "add_tortuosity()"
  )
  ensure_trajectory_grouping(data)
  window_width <- check_tortuosity_input(data, window_width)

  v_cols <- paste0("v_", names(cartesian_axes(data)))

  # Turning angles come from the velocity vectors
  if (!all(c(v_cols, "speed") %in% names(data))) {
    data <- kinematics_cartesian(data) |>
      legacy_kinematics_names()
  }

  windowed_tortuosity(
    data,
    window_width = window_width,
    v_cols = v_cols,
    names = c("straightness", "sinuosity", "emax")
  )
}

#' Straightness, sinuosity and E_max over sliding windows, as
#' `calculate_tortuosity()` computed them
#'
#' Turning angles are between successive velocities, frame by frame.
#'
#' @param data A Cartesian anipoint with velocity columns.
#' @param window_width The window width, an integer of at least 3.
#' @param v_cols The velocity columns, one per axis. Columns whose names
#'   start with `.` are dropped from the result.
#' @param names The names to give straightness, sinuosity and E_max.
#' @return The anipoint with the three measures added.
#' @keywords internal
windowed_tortuosity <- function(data, window_width, v_cols, names) {
  # Store original class for restoration
  original_class <- class(data)

  position_cols <- unname(cartesian_axes(data))
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
      !!names[[1]] := compute_straightness(
        .data$.window_displacement,
        .data$.window_path_length
      ),
      !!names[[2]] := compute_sinuosity(
        .data$.mean_step,
        .data$.mean_cos,
        method = "corrected"
      ),
      !!names[[3]] := compute_emax(.data$.mean_cos)
    ) |>
    dplyr::select(-dplyr::starts_with("."))

  # Restore original class
  class(result) <- original_class
  result
}
