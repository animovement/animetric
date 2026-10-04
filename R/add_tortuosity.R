#' Add tortuosity measures over sliding windows
#'
#' Computes how winding the path is (straightness, sinuosity and E_max) over
#' a window centred on each row, and returns the frame with a column for
#' each. Everything they need, such as the velocities the turning angles come
#' from, is computed internally and not added, so only the three measures
#' appear.
#'
#' @param data A Cartesian anipoint.
#' @param window_width Size of the sliding window, in observations (default
#'   `11L`). Should be an odd number >= 3 for symmetric centering.
#'
#' @return The input anipoint with three columns added, named with the
#'   window width, so that `window_width = 11` gives:
#'   \describe{
#'     \item{`straightness_11`}{Straightness index (D/L), from 0 to 1.}
#'     \item{`sinuosity_11`}{Corrected sinuosity index (Benhamou 2004).}
#'     \item{`e_max_11`}{Maximum expected displacement (dimensionless).}
#'   }
#'   The width in the name keeps these windowed measures apart from the
#'   whole-path `straightness`, `sinuosity` and `e_max` of [summarise_path()],
#'   and lets several window widths sit side by side.
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
#' * [add_kinematics()] for speed, course and the turning measures.
#' * [summarise_path()] for the same measures over each whole trajectory.
#'
#' @export
#'
#' @examples
#' data <- anicore::example_anipoint(n_obs = 30, n_individuals = 1, n_keypoints = 1)
#'
#' data |>
#'   add_tortuosity(window_width = 11)
#'
#' # Several window widths side by side
#' data |>
#'   add_tortuosity(window_width = 5) |>
#'   add_tortuosity(window_width = 11)
add_tortuosity <- function(data, window_width = 11L) {
  ensure_trajectory_grouping(data)
  window_width <- check_tortuosity_input(data, window_width)

  axes <- cartesian_axes(data)
  index <- anicore::get_index(data)
  v_cols <- paste0(".v_", names(axes))
  velocity <- rlang::set_names(
    purrr::map(
      unname(axes),
      \(col) rlang::quo(differentiate(.data[[!!col]], .data[[!!index]]))
    ),
    v_cols
  )

  data |>
    dplyr::mutate(!!!velocity) |>
    windowed_tortuosity(
      window_width = window_width,
      v_cols = v_cols,
      names = paste0(tortuosity_measures, "_", window_width)
    )
}

# The measures add_tortuosity() adds, without their window width
tortuosity_measures <- c("straightness", "sinuosity", "e_max")

#' The columns [add_tortuosity()] has added to a frame
#'
#' Its columns are named `<measure>_<window width>`, one set per width, so
#' they are found by that pattern rather than by a fixed list.
#'
#' @param data A data frame.
#' @return The names of the windowed tortuosity columns, in frame order.
#' @keywords internal
tortuosity_columns <- function(data) {
  pattern <- paste0(
    "^(",
    paste(tortuosity_measures, collapse = "|"),
    ")_[0-9]+$"
  )
  grep(pattern, names(data), value = TRUE)
}

#' Check the input to the windowed tortuosity
#'
#' @param data An anipoint.
#' @param window_width The window width asked for.
#' @param call The calling environment, for error messages.
#' @return `window_width`, as an integer.
#' @keywords internal
check_tortuosity_input <- function(
  data,
  window_width,
  call = rlang::caller_env()
) {
  anicore::ensure_is_anipoint(data)
  if (!anicore::is_cartesian(data)) {
    cli::cli_abort("Data must be in Cartesian coordinates.", call = call)
  }
  window_width <- as.integer(window_width)
  if (window_width < 3L) {
    cli::cli_abort("{.arg window_width} must be at least 3.", call = call)
  }
  window_width
}

#' Straightness, sinuosity and E_max over sliding windows
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
