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
